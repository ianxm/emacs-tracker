;;; tracker.el --- Generate diagrams of personal metrics from diary entries  -*- lexical-binding: t -*-

;; Copyright (C) 2019 Ian Martins

;; Author: Ian Martins <ianxm@jhu.edu>
;; URL: http://github.com/ianxm/emacs-tracker
;; Version: 0.0.1
;; Keywords: docs
;; Package-Requires: ((emacs "24.4"))

;; This file is not part of GNU Emacs.

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; For a full copy of the GNU General Public License
;; see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; tracker.el provides the capability to generate tables and charts
;; from the personal metrics data found in your diary entries.

;;; Code:

(defmacro tracker--min-date (d1 d2)
  "Return the earlier of the given dates D1 and D2."
  `(if (time-less-p ,d1 ,d2) ,d1 ,d2))

(defmacro tracker--max-date (d1 d2)
  "Return the later of the given dates D1 and D2."
  `(if (time-less-p ,d1 ,d2) ,d2 ,d1))

(defmacro tracker--string-to-date (date-string)
  "Get a date value for DATE-STRING."
  `(let ((fields (mapcar #'string-to-number
                      (split-string ,date-string "-"))))
     (encode-time 0 0 0
                  (nth 2 fields)    ; day
                  (nth 1 fields)    ; month
                  (nth 0 fields)))) ; year

(defvar tracker-metric-index nil
  "This is the list of metrics read from the diary file.
It is a list containing: (name count first last) for each metric.
It is cleared when the tracker output buffer is killed, forcing
the diary file to be re-read if the data is needed again.")

(defvar tracker-metric-names (make-vector 5 0)
  "This is an obarray of all existing metric names.")

(defun tracker--process-diary (filter action)
  "Read the diary file.
For each valid metrics entry found, parse the fields and then
apply the given FILTER and ACTION."
  (let (metric-name metric-date metric-value)
    (with-temp-buffer
      (insert-file-contents diary-file)
      (dolist (line (split-string (buffer-string) "\n" t))
        (when (string-match "\\([[:digit:]\-]+\\) \\([[:ascii:]]+\\) \\([[:digit:]\.]+\\)" line) ; valid diary entry
          (setq metric-name (intern (match-string 2 line) tracker-metric-names)
                metric-value (string-to-number (match-string 3 line))
                metric-date (tracker--string-to-date (match-string 1 line))) ; do this last because it (oddly) messes up match data
          (if (funcall filter metric-date metric-name)
              (funcall action metric-date metric-name metric-value)))))))

(defun tracker-clear-data ()
  "Clear the data saved in `tracker-metric-index'.
By clearing it now we force it to be re-read from the diary file
the next time it is needed."
  (when (string= (buffer-name (current-buffer)) "*Tracker Output*")
    (setq tracker-metric-index nil)
    (remove-hook 'kill-buffer-hook #'tracker-clear-data)))

(defun tracker--load-index ()
  "Make sure the metric index has been populated.
This reads the diary file and fills in `tracker-metric-list' if
it is nil."
  (when (not tracker-metric-index)
    (let (metrics
          existing-metric)
      (defun tracker--list-action (date name _value)
        (setq existing-metric (plist-get metrics name))
        (if (not existing-metric)
            (setq metrics (plist-put metrics
                                     name
                                     (list name 1 date date)))
          (setcar (nthcdr 1 existing-metric) (1+ (nth 1 existing-metric)))
          (setcar (nthcdr 2 existing-metric) (tracker--min-date (nth 2 existing-metric) date))
          (setcar (nthcdr 3 existing-metric) (tracker--max-date (nth 3 existing-metric) date))))

      ;; read the diary file, fill `metrics' plist with "name -> (name count first last)"
      (tracker--process-diary (lambda (_date _name) t) ; don't filter out any valid entries
                              'tracker--list-action)

      ;; get the property values from the `metrics' plist
      (let ((metric-iter metrics))
        (while (cdr metric-iter)
          (setq metric-iter (cdr metric-iter)
                tracker-metric-index (nconc tracker-metric-index (list (car metric-iter)))
                metric-iter (cdr metric-iter))))

      ;; sort by last update date
      (setq tracker-metric-index (sort tracker-metric-index
                                       (lambda (a b) (time-less-p (nth 3 b) (nth 3 a)))))
      (add-hook 'kill-buffer-hook #'tracker-clear-data))))

(defun tracker-list ()
  "Display a list of all saved metrics in the output buffer.
This reads the diary file."
  (interactive)

  (tracker--load-index)

  (let ((buffer (get-buffer-create "*Tracker Output*")))
    (set-buffer buffer)
    (read-only-mode -1)
    (erase-buffer)

    ;; write the table
    (insert "| metric | count | first | last | days |\n")
    (insert "|--\n")
    (dolist (metric tracker-metric-index)
      (insert (format "| %s | %s | %s | %s | %s |\n"
                      (nth 0 metric)
                      (nth 1 metric)
                      (format-time-string "%F" (nth 2 metric))
                      (format-time-string "%F" (nth 3 metric))
                      (1+ (- (time-to-days (nth 3 metric))
                         (time-to-days (nth 2 metric)))))))
    (goto-char (point-min))
    (orgtbl-mode t)
    (org-ctrl-c-ctrl-c)

    ;; show output buffer
    (read-only-mode 1)
    (set-window-buffer (selected-window) buffer)))

(defvar tracker-date-grouping-options
  '(day week month year full))

(defvar tracker-value-transform-options
  '(total count percent per-day per-week per-month per-year))

(defun tracker-table ()
  "Get a tabular view of the requested metric."
  (interactive)

  ;; make sure `tracker-metric-index' has been populated
  (tracker--load-index)


  (let ((all-metric-names (mapcar (lambda (metric) (nth 0 metric)) tracker-metric-index))
        metric-name date-grouping value-transform)
    ;; ask for params
    (setq metric-name (intern (completing-read "Metric: " all-metric-names nil t) tracker-metric-names)
          date-grouping (completing-read "Group dates by: " tracker-date-grouping-options nil t nil nil "month")
          value-transform (completing-read "Value transform: " tracker-value-transform-options nil t nil nil "total"))
    (message "params: %s %s %s" date-grouping value-transform metric-name)

    ;; read diary file, filtering for the entries we're interested in
    (defun tracker--table-filter (_date name)
      (eq name metric-name))
    (defun tracker--table-action (date name value)
      (message "%s %s %s" date name value))
    (tracker--process-diary 'tracker--table-filter 'tracker--table-action)))


(provide 'tracker)

;;; tracker.el ends here
