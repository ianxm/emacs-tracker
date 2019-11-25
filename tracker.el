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

(defun tracker--min-date (d1 d2)
  "Returns the earlier of the given dates D1 and D2."
  (if (time-less-p d1 d2) d1 d2))

(defun tracker--max-date (d1 d2)
  "Returns the later of the given dates D1 and D2."
  (if (time-less-p d1 d2) d2 d1))


;; todo can I clear this variable when the output buffer is killed?
(defvar tracker-metric-index nil
  "This is the list of metrics read from the diary file.
It is a list containing: (name count first last) for each
metric.")

(defun tracker--read-metrics (&optional force)
  "This reads the diary file and fills in `tracker-metric-index'.
This only run if `tracker-metric-index' is empty or `force' is non-nil."
  (when (or force
            (not tracker-metric-index))
    (let (metrics metric-name date-fields metric-date existing-metric metric
                  (metric-symbols (make-vector 5 0)))
      (with-temp-buffer
        (insert-file-contents diary-file)
        (dolist (line (split-string (buffer-string) "\n" t))
          ;; build the list of metrics
          (when (string-match "\\([[:digit:]\-]+\\) \\([[:ascii:]]+\\) [[:digit:]\.]+" line) ; valid diary entry
            (setq metric-name (match-string 2 line))
            (setq date-fields (mapcar #'string-to-number (split-string (match-string 1 line) "-")))
            (setq metric-date (encode-time 0 0 0
                                           (nth 2 date-fields)   ; day
                                           (nth 1 date-fields)   ; month
                                           (nth 0 date-fields))) ; year
            (setq existing-metric (plist-get metrics (intern metric-name metric-symbols)))
            (if (not existing-metric)
                (setq metrics (plist-put metrics
                                         (intern metric-name metric-symbols)
                                         (list metric-name 1 metric-date metric-date)))
              (setcar (nthcdr 1 existing-metric) (1+ (nth 1 existing-metric)))
              (setcar (nthcdr 2 existing-metric) (tracker--min-date (nth 2 existing-metric) metric-date))
              (setcar (nthcdr 3 existing-metric) (tracker--max-date (nth 3 existing-metric) metric-date))))))

      ;; now `metrics' is a plist like "name -> (name count first last)"
      ;; get the property values from the plist
      (setq metric metrics)
      (while (cdr metric)
        (setq metric (cdr metric))
        (setq tracker-metric-index (nconc tracker-metric-index (list (car metric))))
        (setq metric (cdr metric)))

      ;; sort by last update date
      (setq tracker-metric-index (sort tracker-metric-index
                                       (lambda (a b) (time-less-p (nth 3 b) (nth 3 a))))))))

(defun tracker-clear ()
  "Clear the data saved in `tracker-metric-index' so it must be
re-read from the diary file."
  (interactive)
  (setq tracker-metric-index nil))

(defun tracker--read-metrics-slow ()
  "This is an attempt o read metrics from the diary file using
`diary-list-entries', but is too slow to use. (~50 days processed
per second)"
  (let ((entries (diary-list-entries '(01 01 2015) 100 t))
        (metric-symbols (make-vector 5 0))
        metrics entry-body metric-name metric-date existing-metric count first last
        (time-start (float-time))
        time-end)
    (dolist (metric entries)
      ;; build the list of metrics
      (setq entry-body (nth 1 metric))
      (when (string-match "\\([[:word:]]+\\) [[:ascii:]\.]+" entry-body) ; valid diary entry
        (setq metric-name (match-string 1 entry-body))
        (setq metric-date (encode-time 0 0 0
                                       (nth 1 (nth 0 metric))
                                       (nth 0 (nth 0 metric))
                                       (nth 2 (nth 0 metric))))
        (setq existing-metric (plist-get metrics (intern metric-name metric-symbols)))
        (if (not existing-metric)
            (setq metrics (plist-put metrics
                                     (intern metric-name metric-symbols)
                                     (list metric-name 1 metric-date metric-date)))
          (setcar (nthcdr 1 existing-metric) (1+ (nth 1 existing-metric)))
          (setcar (nthcdr 2 existing-metric) (tracker--min-date (nth 2 existing-metric) metric-date))
          (setcar (nthcdr 3 existing-metric) (tracker--max-date (nth 3 existing-metric) metric-date)))))
    (message "time %s" (- (float-time) time-start))

    ;; get the property values from the plist
    (setq metric metrics)
    (while (cdr metric)
      (setq metric (cdr metric))
      (setq tracker-metric-index (nconc tracker-metric-index (list (car metric))))
      (setq metric (cdr metric)))

    ;; sort by recency
    (setq tracker-metric-index (sort tracker-metric-index
                                     (lambda (a b) (time-less-p (nth 3 b) (nth 3 a)))))))

(defun tracker-list ()
  "Display a list of all saved metrics in the output buffer.
This reads the diary file."
  (interactive)

  (tracker--read-metrics)               ; read the diary file

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

  ;; make sure we've read the diary file so we know the existing metric names
  (tracker--read-metrics)
  (setq metric-names (mapcar (lambda (metric) (nth 0 metric)) tracker-metric-index))

  ;; ask for params
  (setq metric (completing-read "Metric: " metric-names nil t))
  (setq date-grouping (completing-read "Group dates by: " tracker-date-grouping-options nil t nil nil "month"))
  (setq value-transform (completing-read "Value transform: " tracker-value-transform-options nil t nil nil "total"))
  (message "%s %s %s" date-grouping value-transform metric)

  ;; range may have been passed in from cal, else set it to full

  )


(provide 'tracker)

;;; tracker.el ends here
