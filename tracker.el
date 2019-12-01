;;; tracker.el --- Generate diagrams of personal metrics from diary entries  -*- lexical-binding: t -*-

;; Copyright (C) 2019 Ian Martins

;; Author: Ian Martins <ianxm@jhu.edu>
;; URL: http://github.com/ianxm/emacs-tracker
;; Version: 0.0.1
;; Keywords: docs
;; Package-Requires: ((emacs "24.4") (seq "2.20"))

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

(require 'seq)

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
    (let* (metrics
           existing-metric
           (list-action (lambda (date name _value)
                          (setq existing-metric (plist-get metrics name))
                          (if (not existing-metric)
                              (setq metrics (plist-put metrics
                                                       name
                                                       (list name 1 date date)))
                            (setcar (nthcdr 1 existing-metric) (1+ (nth 1 existing-metric)))
                            (setcar (nthcdr 2 existing-metric) (tracker--min-date (nth 2 existing-metric) date))
                            (setcar (nthcdr 3 existing-metric) (tracker--max-date (nth 3 existing-metric) date))))))

      ;; read the diary file, fill `metrics' plist with "name -> (name count first last)"
      (tracker--process-diary (lambda (_date _name) t) ; don't filter out any valid entries
                              list-action)

      ;; get the property values from the `metrics' plist
      (let ((metric-iter metrics))
        (while (cdr metric-iter)
          (setq metric-iter (cdr metric-iter)
                tracker-metric-index (cons (car metric-iter) tracker-metric-index)
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

  (let ((write-table-fcn (lambda ()
                           (insert "| metric | count | first | last | days |\n") ; header
                           (insert "|--\n")
                           (dolist (metric tracker-metric-index)
                             (insert (format "| %s | %s | %s | %s | %s |\n"      ; data
                                             (nth 0 metric)
                                             (nth 1 metric)
                                             (format-time-string "%F" (nth 2 metric))
                                             (format-time-string "%F" (nth 3 metric))
                                             (1+ (- (time-to-days (nth 3 metric))
                                                    (time-to-days (nth 2 metric))))))))))
    (tracker--write-tracker-output write-table-fcn)))

(defvar tracker-date-grouping-options
  '(day week month year full))

(defvar tracker-value-transform-options
  '(total count percent per-day per-week per-month per-year))


(defun tracker--date-to-bin (date date-grouping)
  "Return the date at the start of the DATE-GROUPING containing DATE."
  (if (eq date-grouping 'full)
      'full
    (let ((date-fields (decode-time date))
          (offset (cond
                   ((eq date-grouping 'day) 0)
                   ((eq date-grouping 'week) (nth 6 (decode-time date)))
                   ((eq date-grouping 'month) (1- (nth 3 (decode-time date))))
                   ((eq date-grouping 'year) (1- (string-to-number (format-time-string "%j" date)))))))
      (encode-time 0 0 0
                   (- (nth 3 date-fields) offset)
                   (nth 4 date-fields)
                   (nth 5 date-fields)))))

(defun tracker--date-to-next-bin (date date-grouping)
  "Return the date at the start of the DATE-GROUPING following the one that starts with DATE."
  (if (eq date-grouping 'full)
      'full
    (let* ((date-fields (decode-time date))
           (is-dst (nth 7 date-fields))
           (next-date-fields date-fields)
           next-date)
      (cond
       ((eq date-grouping 'day) (setcar (nthcdr 3 next-date-fields) (1+ (nth 3 next-date-fields))))
       ((eq date-grouping 'week) (setcar (nthcdr 3 next-date-fields) (+ 7 (nth 3 next-date-fields))))
       ((eq date-grouping 'month) (setcar (nthcdr 4 next-date-fields) (1+ (nth 4 next-date-fields))))
       ((eq date-grouping 'year) (setcar (nthcdr 5 next-date-fields) (1+ (nth 5 next-date-fields)))))
      (setq next-date (apply 'encode-time next-date-fields))
      (setq next-date-fields (decode-time next-date))
      ;; suppress daylight savings shifts
      (when (and (not is-dst)
               (nth 7 next-date-fields))
        (setq next-date (seq-take (time-subtract next-date (seconds-to-time 3600)) 2)))
      (when (and is-dst
                 (not (nth 7 next-date-fields)))
        (setq next-date (seq-take (time-add next-date (seconds-to-time 3600)) 2)))
      ;; return next-date
      next-date)))

(defun tracker--val-to-bin (value value-transform)
  ""
  (cond
   ((eq value-transform 'count) 1)
   ((eq value-transform 'percent) 1)
   (t value)))

(defun tracker--format-bin (date date-grouping)
  "Format the bin start DATE to the bin name used in reporting.

Using the DATE-GROUPING we can eliminate redundant info from bin
names."
  (cond
       ((eq date-grouping 'day) (format-time-string "%F" date))
       ((eq date-grouping 'week) (format-time-string "%F" date))
       ((eq date-grouping 'month) (format-time-string "%Y-%m" date))
       ((eq date-grouping 'year) (format-time-string "%Y" date))))


(defun tracker--bin-to-val (value value-transform date-grouping full-span)
  "Transform and format the bin VALUE into the value used in reporting.

The VALUE-TRANSFORM and DATE-GROUPING are needed to determine how
to transform the value."
  ;; TODO handle partial bin (if current date is first or last and falls within the bin)
  ;; TODO consider looking up days in the particular month
  (let ((bin-duration (cond
                       ((eq date-grouping 'day) 1.0)
                       ((eq date-grouping 'week) 7.0)
                       ((eq date-grouping 'month) 30.0)
                       ((eq date-grouping 'year) 365.0)
                       ((eq date-grouping 'full) (float full-span)))))
    (cond
     ((eq value-transform 'total) value)
     ((eq value-transform 'count) value)
     ((eq value-transform 'percent) (format "%.1f" (* (/ value bin-duration) 100)))
     ((eq value-transform 'per-day) (format "%.1f" (* value (/ 1 bin-duration))))
     ((eq value-transform 'per-week) (format "%.1f" (* value (/ 7 bin-duration))))
     ((eq value-transform 'per-month) (format "%.1f" (* value (/ 30 bin-duration))))
     ((eq value-transform 'per-year) (format "%.1f" (* value (/ 365 bin-duration)))))))

(defun tracker-table ()
  "Get a tabular view of the requested metric."
  (interactive)

  ;; make sure `tracker-metric-index' has been populated
  (tracker--load-index)

  (let ((all-metric-names (mapcar (lambda (metric) (nth 0 metric)) tracker-metric-index))
        (bin-data (make-hash-table :test 'equal))
        metric-name date-grouping value-transform
        sorted-bin-data)

    ;; ask for params
    (setq metric-name (intern (completing-read "Metric: " all-metric-names nil t) tracker-metric-names)
          date-grouping (intern (completing-read "Group dates by: " tracker-date-grouping-options nil t nil nil "month"))
          value-transform (intern (completing-read "Value transform: " tracker-value-transform-options nil t nil nil "total")))
    ;; (message "params: %s %s %s" date-grouping value-transform metric-name)

    (let* ((first-date (nth 2 (nth 0 (seq-filter (lambda (item) (eq (car item) metric-name)) tracker-metric-index))))
           (first-date-bin (tracker--date-to-bin first-date date-grouping))
           (today (tracker--string-to-date (format-time-string "%F")))
           (today-bin (tracker--date-to-bin today date-grouping))
           (full-span (and (eq date-grouping 'full)
                           (- (time-to-days today) (time-to-days first-date))))
           date-bin found-value
           (table-filter-fcn (lambda (_date name)
                           (eq name metric-name)))
           (table-action-fcn (lambda (date _name value)
                                    (setq date-bin (tracker--date-to-bin date date-grouping)
                                          found-value (gethash date-bin bin-data))
                                    (if found-value
                                        (puthash date-bin (+ found-value (tracker--val-to-bin value value-transform)) bin-data)
                                      (puthash date-bin (tracker--val-to-bin value value-transform) bin-data)))))

      (tracker--process-diary table-filter-fcn table-action-fcn)

      ;; fill gaps
      (unless (eq date-grouping 'full)
        (let ((current-date-bin first-date-bin))
          (while (time-less-p current-date-bin today-bin)
            (setq current-date-bin (tracker--date-to-next-bin current-date-bin date-grouping)) ; increment to next bin
            (unless (gethash current-date-bin bin-data)
              (puthash current-date-bin 0 bin-data)))))

      ;; convert to alist and sort
      (maphash (lambda (key value) (setq sorted-bin-data (cons (cons key value) sorted-bin-data))) bin-data)
      (setq sorted-bin-data (sort sorted-bin-data (lambda (a b) (time-less-p (car a) (car b)))))

      ;; scale and print
      (if (eq date-grouping 'full)
          (message "Overall %s %s: %s"
                   metric-name
                   (replace-regexp-in-string "-" " " (symbol-name value-transform))
                   (tracker--bin-to-val (cdar sorted-bin-data) value-transform date-grouping full-span))
        (let ((write-table-fcn (lambda ()
                                 (insert (format "| %s | %s %s |\n" ; header
                                                 date-grouping
                                                 metric-name
                                                 (replace-regexp-in-string "-" " " (symbol-name value-transform))))
                                 (insert "|--\n")
                                 (dolist (bin sorted-bin-data)
                                   (insert (format "| %s | %s |\n"  ; data
                                                   (tracker--format-bin (car bin) date-grouping)
                                                   (tracker--bin-to-val (cdr bin) value-transform date-grouping full-span)))))))
          (tracker--write-tracker-output write-table-fcn))))))

(defun tracker--write-tracker-output (table-fcn)
  "Write tracker output to the output buffer.
This function handles the output buffer and calls TABLE-FCN to
write the table."
  (let ((buffer (get-buffer-create "*Tracker Output*")))
    (set-buffer buffer)
    (read-only-mode -1)
    (erase-buffer)

    (funcall table-fcn)

    (goto-char (point-min))
    (orgtbl-mode t)
    (org-ctrl-c-ctrl-c)

    ;; show output buffer
    (read-only-mode 1)
    (set-window-buffer (selected-window) buffer)))

(provide 'tracker)

;;; tracker.el ends here
