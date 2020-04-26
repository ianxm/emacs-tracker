;;; metrics-tracker.el --- Generate reports of personal metrics from diary entries  -*- lexical-binding: t -*-

;; Copyright (C) 2019-2020 Ian Martins

;; Author: Ian Martins <ianxm@jhu.edu>
;; URL: https://github.com/ianxm/emacs-tracker
;; Version: 0.1.10
;; Keywords: calendar
;; Package-Requires: ((emacs "24.4") (seq "2.3"))

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
;; see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; metrics-tracker.el generates tables and graphs from the personal metrics
;; data found in your diary entries.

;;; Code:

(require 'seq)
(require 'timezone)
(require 'calendar)

(defgroup metrics-tracker nil
  "Options for customizing Metrics Tracker reports."
  :group 'diary
  :tag "Metrics Tracker")

(defcustom metrics-tracker-graph-size '(700 . 500)
  "Specifies the size as (width . height) to be used for graph images."
  :type '(cons integer integer)
  :group 'metrics-tracker)

(defcustom metrics-tracker-metric-name-whitelist nil
  "A list of metric names to include in reports.
If this is specified, only the metrics in this list are
considered.  All others are filtered out.  If this is set, then
`metrics-tracker-metric-name-blacklist' has no effect.

For example: '(\"pushups\" \"situps\")"
  :type '(list :inline t string)
  :group 'metrics-tracker)

(defcustom metrics-tracker-metric-name-blacklist nil
  "A list of metric names to exclude from reports.
This is ignored if `metrics-tracker-metric-name-whitelist' is set.

For example: '(\"pushups\" \"situps\")"
  :type '(list :inline t string)
  :group 'metrics-tracker)

(defcustom metrics-tracker-graph-colors '(("#1f77b4" "#ff7f0e" "#2ca02c" "#d62728" "#9467bd" "#8c564b" "#e377c2" "#7f7f7f" "#bcbd22" "#17becf")
                                          ("#4d871a" "#81871a" "#87581a" "#1a5a87" "#761a87" "#871a1a" "#833a3a" "#403a83" "#3a7f83" "#83743a"))
  "Colors to use for each series in graphs.  The first list is used when in light mode; the second list for dark mode."
  :type '(list (list :inline t string)  ; light mode colors
               (list :inline t string)) ; dark mode colors
  :group 'metrics-tracker)

(defcustom metrics-tracker-dark-mode nil
  "If \"t\", generate graph images with dark backgrounds."
  :type 'boolean
  :group 'metrics-tracker)

(defcustom metrics-tracker-named-reports nil
  "Pre-configured reports that can be easily re-rendered with current data.

All metrics tracker report types are supported.  Add to this list
by generating the report to save and then calling
`metrics-tracker-save-last-report', or by editing this list in
`customize'.

Display a report from this list using `metrics-tracker-show-named-report'."
  :type '(repeat
          (choice (list :tag "Table Report"
                        (string :tag "Report Name")
                        (const :tag "Table Report" table)
                        (repeat :tag "Metric Names" string)
                        (choice :tag "Date Grouping  " (const day) (const week) (const month) (const year) (const full))
                        (choice :tag "Value Transform" (const total) (const count) (const percent) (const :tag "per day" per-day)
                                (const :tag "per week" per-week) (const :tag "per month" per-month) (const :tag "per year" per-year)
                                (const :tag "difference total" diff-total) (const :tag "difference percent" diff-percent) (const :tag "difference per day" diff-per-day)
                                (const :tag "difference per week" diff-per-week) (const :tag "difference per month" diff-per-month) (const :tag "difference per year" diff-per-year))
                        (choice :tag "Start Date     " (const :tag "First occurrence" nil) (string :tag "Date string"))
                        (choice :tag "End Date       " (const :tag "Last occurrence" nil) (string :tag "Date string")))
                  (list :tag "Calendar Report"
                        (string :tag "Report Name")
                        (const :tag "Calendar Report" cal)
                        (repeat :tag "Metric Names" string)
                        (choice :tag "Value Transform" (const total) (const count))
                        (choice :tag "Start Date     " (const :tag "First occurrence" nil) (string :tag "Date string"))
                        (choice :tag "End Date       " (const :tag "Last occurrence" nil) (string :tag "Date string")))
                  (list :tag "Graph Report"
                        (string :tag "Report Name")
                        (const :tag "Graph Report" graph)
                        (repeat :tag "Metric Names" string)
                        (choice :tag "Date Grouping  " (const day) (const week) (const month) (const year) (const full))
                        (choice :tag "Value Transform" (const total) (const count) (const percent) (const :tag "per day" per-day)
                                (const :tag "per week" per-week) (const :tag "per month" per-month) (const :tag "per year" per-year)
                                (const :tag "difference total" diff-total) (const :tag "difference percent" diff-percent) (const :tag "difference per day" diff-per-day)
                                (const :tag "difference per week" diff-per-week) (const :tag "difference per month" diff-per-month) (const :tag "difference per year" diff-per-year))
                        (choice :tag "Start Date     " (const :tag "First occurrence" nil) (string :tag "Date string"))
                        (choice :tag "End Date       " (const :tag "Last occurrence" nil) (string :tag "Date string"))
                        (const :tag "Reserved" nil)
                        (choice :tag "Graph Type     " (const line) (const bar) (const stacked) (const scatter))
                        (choice :tag "Graph Output   " (const ascii) (const svg) (const png)))))

  :group 'metrics-tracker)

(defvar metrics-tracker-metric-index nil
  "This is the list of metrics read from the diary file.
It is a list containing: (name count first last) for each metric.
It is cleared when the metrics-tracker output buffer is killed, forcing
the diary file to be re-read if the data is needed again.")

(defvar metrics-tracker-tempfiles nil
  "This is the list of tempfiles (graph images) that have been created during the current session.")

(defvar metrics-tracker-metric-names (make-vector 5 0)
  "This is an obarray of all existing metric names.")

(defvar metrics-tracker-last-report-config nil
  "This holds the configuration of the last report that was rendered.")

(defconst metrics-tracker-output-buffer-name "*Metrics Tracker Output*"
  "The name of the output buffer.")

(define-error 'metrics-tracker-invalid-value "The given value cannot be parsed")

(defmacro metrics-tracker--min-date (d1 d2)
  "Return the earlier of the given dates D1 and D2."
  `(if (time-less-p ,d1 ,d2) ,d1 ,d2))

(defmacro metrics-tracker--max-date (d1 d2)
  "Return the later of the given dates D1 and D2."
  `(if (time-less-p ,d1 ,d2) ,d2 ,d1))

(defun metrics-tracker--string-to-time (&optional date-str)
  "Return a time value for DATE-STR if given, else for today.
Returned a time value with hours, minutes, seconds zeroed out."
  (apply #'encode-time (mapcar #'(lambda (x) (or x 0)) ; convert nil to 0
                               (seq-take (parse-time-string (or date-str (format-time-string "%F"))) 6))))

(defun metrics-tracker--process-diary (filter action)
  "Parse the diary file.
For each valid metrics entry found, parse the fields and then
apply the given FILTER and ACTION.

Valid metrics entries look like \"DATE TIME METRICNAME VALUE\" where
- DATE looks like \"2020-01-01\" or \"Jan 1, 2020\" or \"1 Jan 2020\"
- TIME (which we ignore) looks like \"10:30\" or \"10:30a\" or \"10:30 am\"
- METRICNAME is any string, whitespace included
- VALUE is a decimal number like \"1\" or \"1.2\" or a duration value like \"10:01\" or \"1:20:32.21\""

  (let ((valid-formats '("^\\([[:digit:]]\\{4\\}\-[[:digit:]]\\{2\\}\-[[:digit:]]\\{2\\}\\) *\\(?:[[:digit:]\:]+ ?[ap]?m?\\)? *\\([[:ascii:]]+\\) \\([[:digit:]\.:]+\\)$"     ; YYYY-MM-DD
                         "^\\([[:digit:]]\\{2\\}\/[[:alpha:]]+\/[[:digit:]]\\{4\\}\\) *\\(?:[[:digit:]\:]+ ?[ap]?m?\\)? *\\([[:ascii:]]+\\) \\([[:digit:]\.:]+\\)$"           ; DD/MMM/YYYY
                         "^\\([[:alpha:]]+ [[:digit:]]\\{1,2\\}, [[:digit:]]\\{4\\}\\) *\\(?:[[:digit:]\:]\\{1,8\\} ?[ap]?m?\\)? *\\([[:ascii:]]+\\) \\([[:digit:]\.:]+\\)$"  ; MMM DD, YYYY
                         "^\\([[:digit:]]\\{1,2\\} [[:alpha:]]+ [[:digit:]]\\{4\\}\\) *\\(?:[[:digit:]\:]\\{1,8\\} ?[ap]?m?\\)? *\\([[:ascii:]]+\\) \\([[:digit:]\.:]+\\)$")) ; DD MMM YYYY
        metric-name metric-date metric-value foundp)
    (with-temp-buffer
      (insert-file-contents diary-file)
      (dolist (line (split-string (buffer-string) "\n" t))
        (condition-case nil
            (progn
              (setq foundp nil)
              (dolist (format valid-formats)
                (if (string-match format line)
                    (setq metric-date (apply #'encode-time (mapcar #'(lambda (x) (or x 0)) ; convert nil to 0
                                                                   (seq-take (parse-time-string (match-string 1 line)) 6)))
                          metric-name (intern (match-string 2 line) metrics-tracker-metric-names)
                          metric-value (metrics-tracker--try-read-value (match-string 3 line))
                          foundp t)))
              (if (and foundp (funcall filter metric-date metric-name))
                  (funcall action metric-date metric-name metric-value)))
          (metrics-tracker-invalid-value nil) ; the regexes aren't strict enough to filter this out, but it should be skipped
          (error (message "error parsing line: %s" line)))))))

(defun metrics-tracker--try-read-value (string-value)
  "Read a value from the given string, or signal that no value can be read.

Any string that matches the `valid-formats' regex can end up
here, but not all can be parsed (for example \"10::21\").  In
those cases we raise the `metric-tracker-invalid-value' signal.

STRING-VALUE is a string containing a metric value.  It may be
formatted as a number (10.21) or a duration (10:21). Hours are
optional for duration values."
  (cond ((string-match "^\\(?:\\([[:digit:]]\\{1,2\\}\\):\\)?\\([[:digit:]]\\{2\\}\\):\\([[:digit:]]\\{2\\}\\(?:\\.[[:digit:]]*\\)?\\)$" string-value) ; duration as hh:mm:ss.ms
         (let ((h (if (match-string 1 string-value) (string-to-number (match-string 1 string-value)) 0))
               (m (string-to-number (match-string 2 string-value)))
               (s (string-to-number (match-string 3 string-value))))
           (+ h (/ m 60.0) (/ s 3600.0)))) ; return duration in hours
        ((string-match "^[[:digit:]\.]+$" string-value) ; number like 10.21
         (string-to-number string-value))
        (t ; skip
         (signal 'metrics-tracker-invalid-value string-value))))

(defun metrics-tracker-clear-data ()
  "Clear cached data and delete tempfiles.
Clear the data cached in `metrics-tracker-metric-index' in order to force
it to be re-read from the diary file the next time it is
needed.  Also delete the tempfiles (graph images) listed in
`metrics-tracker-tempfiles'."
  (when (string= (buffer-name (current-buffer)) metrics-tracker-output-buffer-name)
    (setq metrics-tracker-metric-index nil)
    (metrics-tracker-remove-tempfiles)
    (remove-hook 'kill-buffer-hook #'metrics-tracker-clear-data)))

(defun metrics-tracker-remove-tempfiles ()
  "Remove any tempfiles (graph images) that were created during the current session."
  (dolist (elt metrics-tracker-tempfiles)
    (if (file-exists-p elt)
        (delete-file elt)))
  (setq metrics-tracker-tempfiles nil)
  (remove-hook 'kill-emacs-hook #'metrics-tracker-remove-tempfiles))

(defun metrics-tracker--load-index ()
  "Make sure the metric index has been populated.
This reads the diary file and populated in
`metrics-tracker-metric-list' if it is nil.

`metrics-tracker-metric-list' is a list of
\(metric-name count first last daysago) sorted by 'last'."
  (unless metrics-tracker-metric-index
    (let* (metrics ; will contain plist of metric-name -> (metric-name count first last daysago)
           existing-metric
           (today (metrics-tracker--string-to-time))
           (list-filter-fcn (cond ((not (null metrics-tracker-metric-name-whitelist))
                                   (lambda (_date name) ; filter out non-whitelisted metrics
                                     (seq-contains metrics-tracker-metric-name-whitelist (symbol-name name))))
                                  ((not (null metrics-tracker-metric-name-blacklist))
                                   (lambda (_date name) ; filter out blacklisted metrics
                                     (not (seq-contains metrics-tracker-metric-name-blacklist (symbol-name name)))))
                                  (t                    ; keep all metrics
                                   (lambda (_date _name) t))))
           (list-action-fcn (lambda (date name _value)
                              (setq existing-metric (plist-get metrics name))
                              (if (not existing-metric)
                                  (setq metrics (plist-put metrics name
                                                           (list name 1 date date (- (time-to-days today)
                                                                                     (time-to-days date)))))
                                (setcar (nthcdr 1 existing-metric) (1+ (nth 1 existing-metric)))
                                (setcar (nthcdr 2 existing-metric) (metrics-tracker--min-date (nth 2 existing-metric) date))
                                (setcar (nthcdr 3 existing-metric) (metrics-tracker--max-date (nth 3 existing-metric) date))
                                (setcar (nthcdr 4 existing-metric) (- (time-to-days today)
                                                                      (time-to-days (nth 3 existing-metric))))))))

      ;; read the diary file, fill `metrics' plist with "name -> (name count first last)"
      (metrics-tracker--process-diary list-filter-fcn list-action-fcn)

      ;; get the property values from the `metrics' plist
      (let ((metric-iter metrics))
        (while (cdr metric-iter)
          (setq metric-iter (cdr metric-iter)
                metrics-tracker-metric-index (cons (car metric-iter) metrics-tracker-metric-index)
                metric-iter (cdr metric-iter))))

      ;; sort by last update date
      (setq metrics-tracker-metric-index (sort metrics-tracker-metric-index (lambda (a b) (> (nth 4 b) (nth 4 a)))))
      (add-hook 'kill-buffer-hook #'metrics-tracker-clear-data))))

(defmacro metrics-tracker--num-sort (col)
  "Sort string numbers in column COL of a tabulated list."
  `(lambda (x y) (< (string-to-number (elt (nth 1 x) ,col))
                    (string-to-number (elt (nth 1 y) ,col)))))

(defmacro metrics-tracker--sort-dates (dates)
  "Sort a list of DATES."
  `(sort ,dates (lambda (a b) (time-less-p a b))))

(defmacro metrics-tracker--validate-input (variable choice options)
  "Validate that VARIABLE was set to a CHOICE that is among the valid OPTIONS."
  `(if (not (seq-contains ,options ,choice))
       (error (concat ,variable " must be one of: " (mapconcat #'symbol-name ,options ", ")))
     t))

;;;###autoload
(defun metrics-tracker-list ()
  "Display a list of all saved metrics in the output buffer.
This reads the diary file."
  (interactive)

  (metrics-tracker--load-index)

  (metrics-tracker--setup-output-buffer)
  (tabulated-list-mode)

  ;; set headers
  (let ((metric-name-width (seq-reduce (lambda (width ii) (max width (length ii)))
                                       (mapcar (lambda (ii) (symbol-name (nth 0 ii))) metrics-tracker-metric-index)
                                       10)))
    (setq-local tabulated-list-format (vector (list "metric" metric-name-width t)
                                              (list "days ago" 10 (metrics-tracker--num-sort 1))
                                              (list "first" 12 t)
                                              (list "last" 12 t)
                                              (list "count" 8 (metrics-tracker--num-sort 4)))))
  ;; configure
  (setq-local tabulated-list-padding 2)
  (setq-local tabulated-list-sort-key (cons "days ago" nil))

  ;; populate the table data
  (let (data)
    (dolist (metric metrics-tracker-metric-index)
      (setq data (cons (list (symbol-name (nth 0 metric))
                             (vector (symbol-name (nth 0 metric))
                                     (number-to-string (nth 4 metric))
                                     (format-time-string "%F" (nth 2 metric))
                                     (format-time-string "%F" (nth 3 metric))
                                     (number-to-string (nth 1 metric))))
                       data)))
    (setq-local tabulated-list-entries data))

  ;; render the table
  (tabulated-list-init-header)
  (tabulated-list-print nil nil)

  (metrics-tracker--show-output-buffer))

(defconst metrics-tracker-grouping-and-transform-options
  '(day (total count)
        week (total count percent per-day diff-total diff-percent diff-per-day)
        month (total count percent per-day per-week diff-total diff-percent diff-per-day diff-per-week)
        year (total count percent per-day per-week per-month diff-total diff-percent diff-per-day diff-per-week diff-per-month)
        full (total count percent per-day per-week per-month per-year diff-total diff-percent diff-per-day diff-per-week diff-per-month diff-per-year))
  "This is a plist of date-grouping options mapped to value-transform options.")

(defun metrics-tracker--date-grouping-options ()
  "Pull the list of date-grouping options out of `metrics-tracker-grouping-and-transform-options'."
  (seq-filter (lambda (x) (symbolp x)) metrics-tracker-grouping-and-transform-options))

(defun metrics-tracker--value-transform-options (date-grouping)
  "Return the valid value-transforms for the given DATE-GROUPING."
  (plist-get metrics-tracker-grouping-and-transform-options date-grouping))

(defconst metrics-tracker-graph-options '(line bar stacked scatter)
  "The types of supported graphs.")

(defun metrics-tracker--graph-options (date-transform)
  "Return the valid graph-options for the given DATE-TRANSFORM.
This only needs to be done for graphs, since line and scatter
graphs don't work if there's just one data point."
  (if (eq date-transform 'full)
      (seq-difference metrics-tracker-graph-options '(line scatter))
    metrics-tracker-graph-options))

(defconst metrics-tracker-graph-output-options '(ascii svg png)
  "The graph output options.")

(defun metrics-tracker--presorted-options (options)
  "Prevent Emacs from sorting OPTIONS.
For some reason some versions of Emacs sort the given options instead of just
presenting them.  Solution taken from:
https://emacs.stackexchange.com/questions/41801/how-to-stop-completing-read-ivy-completing-read-from-sorting"
  (lambda (string pred action)
    (if (eq action 'metadata)
        '(metadata (display-sort-function . identity)
                   (cycle-sort-function . identity))
      (complete-with-action
       action options string pred))))

(defun metrics-tracker--date-to-bin (date date-grouping)
  "Return the start date of the bin containing DATE of size DATE-GROUPING."
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

(defun metrics-tracker--date-to-next-bin (date date-grouping)
  "Return the start date at the bin following the bin containing DATE of size DATE-GROUPING."
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
      (setq next-date (apply #'encode-time next-date-fields))
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

(defun metrics-tracker--val-to-bin (value value-transform)
  "Convert the VALUE to be stored for the bin based on VALUE-TRANSFORM.
Either save the total value or a count of occurrences."
  (cond
   ((eq value-transform 'count) 1)
   ((or (eq value-transform 'percent)
        (eq value-transform 'diff-percent)) 1)
   (t value)))

(defun metrics-tracker--format-bin (date-grouping)
  "Get the format string for the the bin based on the DATE-GROUPING."
  (cond
   ((eq date-grouping 'day) "%Y-%m-%d")
   ((eq date-grouping 'week) "%Y-%m-%d")
   ((eq date-grouping 'month) "%Y-%m")
   ((eq date-grouping 'year) "%Y")))

(defun metrics-tracker--trim-duration (span bin-start first-date last-date)
  "Trim the given duration if it falls outside of first and last dates.

If part of the bin SPAN days long and starting at BIN-START falls
outside of FIRST-DATE or LAST-DATE, then trim it to include the
days within the bin and inside of FIRST-DATE and LAST-DATE."
  (let ((bin-end (time-add bin-start (seconds-to-time (* span 86400)))))
    (cond
     ((time-less-p bin-start first-date) ; bin starts before first occurrence
      (float (max 0
                  (- (time-to-days bin-end)
                     (time-to-days first-date)))))
     ((time-less-p last-date bin-end)    ; bin ends after last occurrence
      (float (1+ (- (time-to-days last-date)
                    (time-to-days bin-start)))))
     (t
      (float span)))))

(defun metrics-tracker--days-of-month (date)
  "Find the number of days in the month containing DATE.  This depends on `timezeone'."
  (let ((date-fields (decode-time date)))
    (timezone-last-day-of-month (nth 4 date-fields) (nth 5 date-fields))))

(defun metrics-tracker--bin-to-val (value
                            value-transform date-grouping
                            bin-date first-date today)
  "Transform and format the bin VALUE into the value used in reporting.

The VALUE-TRANSFORM and DATE-GROUPING are needed to determine how
to transform the value.  BIN-DATE, FIRST-DATE, TODAY are all
needed to determine the number of days in the current bin."
  (let ((bin-duration (cond
                       ((eq date-grouping 'day) 1.0)
                       ((eq date-grouping 'week) (metrics-tracker--trim-duration 7 bin-date first-date today))
                       ((eq date-grouping 'month) (metrics-tracker--trim-duration (metrics-tracker--days-of-month bin-date) bin-date first-date today))
                       ((eq date-grouping 'year) (metrics-tracker--trim-duration 365 bin-date first-date today))
                       ((eq date-grouping 'full) (float (- (time-to-days today)
                                                           (time-to-days first-date)))))))
    (cond
     ((or (eq value-transform 'total)
          (eq value-transform 'diff-total))
      value)

     ((eq value-transform 'count)
      value)

     ((or (eq value-transform 'percent)
          (eq value-transform 'diff-percent))
      (* (/ value bin-duration) 100))

     ((or (eq value-transform 'per-day)
          (eq value-transform 'diff-per-day))
      (* value (/ 1 bin-duration)))

     ((or (eq value-transform 'per-week)
          (eq value-transform 'diff-per-week))
      (* value (/ 7 bin-duration)))

     ((or (eq value-transform 'per-month)
          (eq value-transform 'diff-per-month))
      (* value (/ 30 bin-duration)))

     ((or (eq value-transform 'per-year)
          (eq value-transform 'diff-per-year))
      (* value (/ 365 bin-duration))))))

(defun metrics-tracker--bin-metric-data (metric-name date-grouping value-transform today &optional allow-gaps-p)
  "Read the requested metric data from the diary.
Only keep entries for METRIC-NAME.  Apply DATE-GROUPING and
VALUE-TRANSFORM.  Fill gaps ending at TODAY.  Return the sorted
bin data as (list (date . pretransformed-value)).  If
ALLOW-GAPS-P is t, don't fill in gaps."
  (let* ((bin-data (make-hash-table :test 'equal))
         (first-date (nth 2 (nth 0 (seq-filter (lambda (item) (eq (car item) metric-name)) metrics-tracker-metric-index))))
         (first-date-bin (metrics-tracker--date-to-bin first-date date-grouping))
         (today-bin (metrics-tracker--date-to-bin today date-grouping))
         date-bin found-value
         (table-filter-fcn (lambda (_date name)
                             (eq name metric-name)))
         (table-action-fcn (lambda (date _name value)
                             (setq date-bin (metrics-tracker--date-to-bin date date-grouping)
                                   found-value (gethash date-bin bin-data))
                             (if found-value
                                 (puthash date-bin (+ found-value (metrics-tracker--val-to-bin value value-transform)) bin-data)
                               (puthash date-bin (metrics-tracker--val-to-bin value value-transform) bin-data)))))

    (metrics-tracker--process-diary table-filter-fcn table-action-fcn)

    ;; fill gaps and apply value transforms
    (if (eq date-grouping 'full)
        (let ((write-value (gethash 'full bin-data)))
          (puthash 'full
                   (metrics-tracker--bin-to-val (format "%s" (/ (round (* write-value 100)) 100.0))
                                                value-transform date-grouping
                                                'full first-date today)
                   bin-data))
      (let* ((current-date-bin first-date-bin)
             (last-value (metrics-tracker--bin-to-val (gethash current-date-bin bin-data)
                                                      value-transform date-grouping
                                                      current-date-bin first-date today)) ; the value from the last bin we visited
             current-value                                    ; the value from the bin we're currently visiting
             write-value)                                     ; the value to write for the current bin
        (while (or (time-less-p current-date-bin today-bin)
                   (equal current-date-bin today-bin))
          (when (or (gethash current-date-bin bin-data)
                    (not allow-gaps-p))
            (setq current-value (metrics-tracker--bin-to-val (gethash current-date-bin bin-data 0)
                                                             value-transform date-grouping
                                                             current-date-bin first-date today))
            (if (or (eq value-transform 'diff-total)
                    (eq value-transform 'diff-percent)
                    (eq value-transform 'diff-per-day)
                    (eq value-transform 'diff-per-week)
                    (eq value-transform 'diff-per-month)
                    (eq value-transform 'diff-per-year))
                (setq write-value (- current-value last-value)) ; apply diff
              (setq write-value current-value))

            (puthash current-date-bin
                     (format "%s" (/ (round (* write-value 100)) 100.0))
                     bin-data)) ; replace number with a string
          (setq last-value current-value
                current-date-bin (metrics-tracker--date-to-next-bin current-date-bin date-grouping))))) ; increment to next bin
    bin-data))

(defun metrics-tracker--setup-output-buffer ()
  "Create and clear the output buffer."
  (let ((buffer (get-buffer-create metrics-tracker-output-buffer-name)))
    (set-buffer buffer)
    (read-only-mode)
    (let ((inhibit-read-only t))
      (erase-buffer))))

(defun metrics-tracker--show-output-buffer ()
  "Show the output buffer."
  (let ((buffer (get-buffer metrics-tracker-output-buffer-name)))
    (set-window-buffer (selected-window) buffer)))

(defun metrics-tracker--check-gnuplot-exists ()
  "Check if gnuplot is installed on the system."
  (unless (eq 0 (call-process-shell-command "gnuplot --version"))
    (error "Cannot find gnuplot")))

(defun metrics-tracker--ask-for-metrics (multp)
  "Prompt for metric names.
If MULTP is false, only ask for one metric, else loop until
\"no more\" is chosen.  Return the selected list of metric names."
  (let* ((all-metric-names (mapcar (lambda (metric) (nth 0 metric)) metrics-tracker-metric-index))
         (last-metric-name (completing-read "Metric: " (metrics-tracker--presorted-options all-metric-names) nil t))
         (metric-names (cons last-metric-name nil)))
    (setq all-metric-names (cons "no more" all-metric-names))
    (while (and (not (string= last-metric-name "no more"))
                multp)
      (setq all-metric-names (seq-remove (lambda (elt) (string= elt last-metric-name)) all-metric-names)
            last-metric-name (completing-read "Metric: " (metrics-tracker--presorted-options all-metric-names) nil t))
      (if (not (string= last-metric-name "no more"))
          (setq metric-names (cons last-metric-name metric-names))))
    (reverse metric-names)))

(defun metrics-tracker--ask-for-date (prompt)
  "Display the PROMPT, return the response or nil if no response given."
  (let ((date-str (read-string prompt)))
    (if (string= "" date-str) nil date-str)))

;;;###autoload
(defun metrics-tracker-table (arg)
  "Interactive way to get a tabular view of the requested metric.

This function gets user input and then delegates to
`metrics-tracker-table-render'.

ARG is optional, but if given and it is the default argument,
allow selection of multiple metrics and date ranges."
  (interactive "P")

  ;; make sure `metrics-tracker-metric-index' has been populated
  (metrics-tracker--load-index)

  (let* ((ivy-sort-functions-alist nil)
         ;; ask for params
         (extrap (equal arg '(4)))
         (metric-names-str (metrics-tracker--ask-for-metrics extrap))
         (date-grouping (intern (completing-read "Group dates by: " (metrics-tracker--presorted-options
                                                                     (metrics-tracker--date-grouping-options))
                                                 nil t nil nil "month")))
         (value-transform (intern (completing-read "Value transform: " (metrics-tracker--presorted-options
                                                                        (metrics-tracker--value-transform-options date-grouping))
                                                   nil t nil nil "total")))
         (start-date (and extrap (metrics-tracker--ask-for-date "Start date (optional): ")))
         (end-date (and extrap (metrics-tracker--ask-for-date "End date (optional): "))))
    (metrics-tracker-table-render (list metric-names-str date-grouping value-transform
                                        start-date end-date))))

;;;###autoload
(defun metrics-tracker-table-render (table-config)
  "Programmatic way to get a tabular view of the requested metric.

TABLE-CONFIG should be a list of all inputs needed to render a table.
The first item in the list is a list of metric names as strings.
The second item is a date grouping as a symbol.
The third item is a value transform as a symbol.
The fourth item (optional) is a date string, ignore occurrences before.
The fifth item (optional) is a date string, ignore occurrences after.
Date strings must be in YYYY-MM-DD format.

For example:
    '((\"metricname\") year total nil nil)"

  ;; make sure `metrics-tracker-metric-index' has been populated
  (metrics-tracker--load-index)

  (let* ((today (metrics-tracker--string-to-time))
         (all-metric-names (mapcar (lambda (metric) (nth 0 metric)) metrics-tracker-metric-index))
         (metric-names-str (nth 0 table-config))
         (metric-names (mapcar (lambda (name) (intern name metrics-tracker-metric-names)) metric-names-str))
         (date-grouping (nth 1 table-config))
         (value-transform (nth 2 table-config))
         (start-date (and (nth 3 table-config)
                          (metrics-tracker--string-to-time (nth 3 table-config))))
         (end-date (and (nth 4 table-config)
                          (metrics-tracker--string-to-time (nth 4 table-config))))
         bin-data-all)

    ;; validate inputs
    (dolist (metric metric-names t)
      (metrics-tracker--validate-input "metric" metric all-metric-names))
    (metrics-tracker--validate-input "date-grouping" date-grouping (metrics-tracker--date-grouping-options))
    (metrics-tracker--validate-input "value-transform" value-transform (metrics-tracker--value-transform-options date-grouping))
    (unless (or (null start-date) (null end-date) (time-less-p start-date end-date))
      (error "The end date is before the start date"))

    ;; save the config
    (setq metrics-tracker-last-report-config (cons 'table table-config))

    ;; load metric data into bins; list of `bin-data' for each metric in the same order as `metric-names'
    (setq bin-data-all (mapcar (lambda (metric-name) (metrics-tracker--bin-metric-data metric-name date-grouping value-transform today))
                               metric-names))

    (if (and (eq date-grouping 'full)
             (= 1 (length metric-names)))
        ;; if there's only one value to print, just write it to the status line
        (message "Overall %s %s: %s"
                 (car metric-names)
                 (replace-regexp-in-string "-" " " (symbol-name value-transform))
                 (gethash 'full (car bin-data-all)))

      ;; otherwise write a table
      (metrics-tracker--setup-output-buffer)
      (tabulated-list-mode)

      ;; set table headers
      (setq tabulated-list-format
            (vconcat (list (list (symbol-name date-grouping) 12 t))
                     (mapcar (lambda (metric-name)
                               (let ((label (format "%s %s" metric-name (replace-regexp-in-string "-" " " (symbol-name value-transform)))))
                                 (list label (max 10 (1+ (length label))) (metrics-tracker--num-sort 1))))
                             metric-names-str)))

      ;; configure
      (setq tabulated-list-padding 2)
      (setq tabulated-list-sort-key (cons (symbol-name date-grouping) nil))

      ;; compute and set table data
      (setq-local tabulated-list-entries (metrics-tracker--compute-data bin-data-all date-grouping start-date end-date nil))

      ;; render the table
      (let ((inhibit-read-only t))
        (tabulated-list-init-header)
        (tabulated-list-print nil nil))

      (metrics-tracker--show-output-buffer))))

(defun metrics-tracker--compute-data (bin-data-all date-grouping start-date end-date graph-type)
  "Return a list of '(date-str [date-str metric1 metric2 metric3]).

BIN-DATA-ALL is a list of hashtables containing bin data for each metric.
DATE-GROUPING is the selected bin size.
START-DATE if given, filter occurrences before.
END-DATE if given, filter occurrences after.
GRAPH-TYPE is the selected graph type, if the current operation is a graph."
  (let ((gap-value (cond ((or (eq graph-type 'line) (eq graph-type 'scatter)) ".") ; line or scatter use "." for gaps
                         ((not (null graph-type)) "0") ; other graphs use "0" for gaps
                         (t "" ))) ; tables use "" for gaps
        merged-dates date-str data)

    ;; merge dates across metrics and sort
    (setq merged-dates (seq-reduce (lambda (dates ii) (append (hash-table-keys ii) dates))
                                   bin-data-all
                                   nil))
    (delete-dups merged-dates)
    (setq merged-dates (metrics-tracker--sort-dates merged-dates))

    ;; pull data for each metric for each date (builds a list backwards)
    (dolist (date merged-dates)
      (when (and (or (null start-date) ; only use occurrences between `start-date' and `end-date', inclusive
                     (not (time-less-p date start-date)))
                 (or (null end-date)
                     (not (time-less-p end-date date))))
        (setq date-str (if (eq date-grouping 'full)
                           "full"
                         (format-time-string (metrics-tracker--format-bin date-grouping) date))
              data (cons (list date-str
                               (vconcat (list date-str)
                                        (mapcar (lambda (bin-data) (gethash date bin-data gap-value)) bin-data-all)))
                         data))))
    (reverse data))) ; return data

;;;###autoload
(defun metrics-tracker-cal (arg)
  "Interactive way to get a calendar view of a requested metric.

This function gets user input and then delegates to
`metrics-tracker-cal-render'.

ARG is optional, but if given and it is the default argument,
allow selection of date ranges."
  (interactive "P")

  ;; make sure `metrics-tracker-metric-index' has been populated
  (metrics-tracker--load-index)

  (let* ((ivy-sort-functions-alist nil)
         ;; ask for params
         (extrap (equal arg '(4)))
         (all-metric-names (mapcar (lambda (metric) (nth 0 metric)) metrics-tracker-metric-index))
         (metric-name-str (completing-read "Metric: " (metrics-tracker--presorted-options all-metric-names) nil t))
         (value-transform (intern (completing-read "Value transform: " (metrics-tracker--presorted-options
                                                                        (metrics-tracker--value-transform-options 'day))
                                                   nil t nil nil "total")))
         (start-date (and extrap (metrics-tracker--ask-for-date "Start date (optional): ")))
         (end-date (and extrap (metrics-tracker--ask-for-date "End date (optional): "))))
    (metrics-tracker-cal-render (list metric-name-str value-transform start-date end-date))))

;;;###autoload
(defun metrics-tracker-cal-render (cal-config)
  "Programmatic way to get a calendar view of a requested metric.

CAL-CONFIG should be a list of all inputs needed to generate the calendar.
The first item in the list is a metric name as a string.
The second item is a value transform as a symbol.
The third item (optional) is a date string, ignore occurrences before.
The fourth item (optional) is a date string, ignore occurrences after.
Date strings must be in YYYY-MM-DD format.

For example:
    '(\"metricname\" total nil nil)"

  ;; make sure `metrics-tracker-metric-index' has been populated
  (metrics-tracker--load-index)

  (let* ((today (metrics-tracker--string-to-time))
         (all-metric-names (mapcar (lambda (metric) (nth 0 metric)) metrics-tracker-metric-index))
         (metric-name (intern (nth 0 cal-config) metrics-tracker-metric-names))
         (value-transform (nth 1 cal-config))
         (start-date (and (nth 2 cal-config)
                          (metrics-tracker--string-to-time (nth 2 cal-config))))
         (end-date (and (nth 3 cal-config)
                          (metrics-tracker--string-to-time (nth 3 cal-config)))))

    ;; validate inputs
    (metrics-tracker--validate-input "metric" metric-name all-metric-names)
    (metrics-tracker--validate-input "value-transform" value-transform (metrics-tracker--value-transform-options 'day))
    (unless (or (null start-date) (null end-date) (time-less-p start-date end-date))
      (error "The end date is before the start date"))

    ;; save the config
    (setq metrics-tracker-last-report-config (cons 'cal cal-config))

    (metrics-tracker--setup-output-buffer)
    (fundamental-mode)

    ;; render the calendars
    (let* ((inhibit-read-only t)
           ;; load metric data into bins
           (bin-data (metrics-tracker--bin-metric-data metric-name 'day value-transform today t))
           (dates (hash-table-keys bin-data))
           ;; find the first date
           (first (or start-date
                      (seq-reduce (lambda (first ii) (if (time-less-p first ii) first ii))
                                  dates (car dates))))
           (first-decoded (decode-time first))
           (month (nth 4 first-decoded))
           (year (nth 5 first-decoded))
           ;; find the month on which to end
           (end (or end-date today))
           (end-decoded (decode-time end))
           (end-month (nth 4 end-decoded))
           (end-year (nth 5 end-decoded)))
      (insert (format "  %s\n\n" (symbol-name metric-name)))
      (put-text-property (point-min) (point-max) 'face 'bold)
      (while (or (< year end-year)
                 (and (= year end-year)
                      (<= month end-month)))
        (metrics-tracker--print-month month year bin-data first end)
        (insert "\n\n\n")
        (setq month (1+ month))
        (when (> month 12)
          (setq month 1
                year (1+ year)))))

    (metrics-tracker--show-output-buffer)))

(defun metrics-tracker--print-month (month year bin-data first today)
  "Write metric data as a calendar for MONTH of YEAR.
BIN-DATA contains the data to render, in a hashtable as \"time ->
value\".  It ranges from FIRST until TODAY, which are time
values."
  (let ((first-day (encode-time 0 0 0 1 month year))
        day)
    (insert (format "                    %s\n\n" (format-time-string "%b %Y" first-day)))
    (insert "      Su    Mo    Tu    We    Th    Fr    Sa\n  ")
    (dotimes (_ii (nth 6 (decode-time first-day)))
      (insert "      "))
    (dotimes (daynum (metrics-tracker--days-of-month first-day))
      (setq day (encode-time 0 0 0 (1+ daynum) month year))
      (cond ((gethash day bin-data) (insert (format "%6s" (gethash day bin-data))))
            ((time-less-p day first) (insert "     _")) ; before data
            ((time-less-p today day) (insert "     _")) ; after today
            (t (insert "     .")))                      ; gap in data
      (when (= (nth 6 (decode-time day)) 6)
          (insert "\n  ")))))

;;;###autoload
(defun metrics-tracker-graph (arg)
  "Interactive way to get a graph of requested metrics.

This function gets user input and then delegates to
`metrics-tracker-graph-render'.

ARG is optional, but if given and it is the default argument,
allow selection of multiple metrics and date ranges."
  (interactive "P")

  (metrics-tracker--check-gnuplot-exists)

  ;; make sure `metrics-tracker-metric-index' has been populated
  (metrics-tracker--load-index)

  (let* ((ivy-sort-functions-alist nil)
         ;; ask for params
         (extrap (equal arg '(4)))
         (metric-names-str (metrics-tracker--ask-for-metrics extrap))
         (date-grouping (intern (completing-read "Group dates by: " (metrics-tracker--presorted-options
                                                                     (metrics-tracker--date-grouping-options))
                                                 nil t nil nil "month")))
         (value-transform (intern (completing-read "Value transform: " (metrics-tracker--presorted-options
                                                                        (metrics-tracker--value-transform-options date-grouping))
                                                   nil t nil nil "total")))
         (start-date (and extrap (metrics-tracker--ask-for-date "Start date (optional): ")))
         (end-date (and extrap (metrics-tracker--ask-for-date "End date (optional): ")))
         (graph-type (intern (completing-read "Graph type: " (metrics-tracker--presorted-options
                                                              (metrics-tracker--graph-options date-grouping))
                                              nil t)))
         (graph-output (intern (completing-read "Graph output: " (metrics-tracker--presorted-options
                                                                  metrics-tracker-graph-output-options)
                                                nil t nil nil "ascii"))))

    (metrics-tracker-graph-render (list metric-names-str date-grouping value-transform
                                        start-date end-date nil graph-type graph-output))))

;;;###autoload
(defun metrics-tracker-graph-render (graph-config)
  "Programmatic way to get a graph of the requested metric.

GRAPH-CONFIG should be a list of all inputs needed to render a graph.
The first item in the list is a list of metric names as strings.
The second item is a date grouping as a symbol.
The third item is a value transform as a symbol.
The fourth item (optional) is a date string, ignore occurrences before.
The fifth item (optional) is a date string, ignore occurrences after.
The sixth item is reserved.
The seventh item is the graph type as a symbol.
The eighth item is the graph output option as a symbol.
Date strings must be in YYYY-MM-DD format.

For example:
    '((\"metricname\") year total nil nil nil line svg)"

  (metrics-tracker--check-gnuplot-exists)

  ;; make sure `metrics-tracker-metric-index' has been populated
  (metrics-tracker--load-index)

  (let* ((ivy-sort-functions-alist nil)
         (today (metrics-tracker--string-to-time))
         ;; ask for params
         (all-metric-names (mapcar (lambda (metric) (nth 0 metric)) metrics-tracker-metric-index))
         (metric-names-str (nth 0 graph-config))
         (metric-names (mapcar (lambda (name) (intern name metrics-tracker-metric-names)) metric-names-str))
         (date-grouping (nth 1 graph-config))
         (value-transform (nth 2 graph-config))
         (start-date (and (nth 3 graph-config)
                          (metrics-tracker--string-to-time (nth 3 graph-config))))
         (end-date (and (nth 4 graph-config)
                          (metrics-tracker--string-to-time (nth 4 graph-config))))
         (graph-type (nth 6 graph-config))
         (graph-output (nth 7 graph-config))
         buffer fname)

    (dolist (metric metric-names t)
      (metrics-tracker--validate-input "metric" metric all-metric-names))
    (metrics-tracker--validate-input "date-grouping" date-grouping (metrics-tracker--date-grouping-options))
    (metrics-tracker--validate-input "value-transform" value-transform (metrics-tracker--value-transform-options date-grouping))
    (metrics-tracker--validate-input "graph-type" graph-type (metrics-tracker--graph-options date-grouping))
    (metrics-tracker--validate-input "graph-output" graph-output metrics-tracker-graph-output-options)
    (unless (or (null start-date) (null end-date) (time-less-p start-date end-date))
      (error "The end date is before the start date"))

    ;; save the config
    (setq metrics-tracker-last-report-config (cons 'graph graph-config))

    ;; prep output buffer
    (setq buffer (get-buffer-create metrics-tracker-output-buffer-name)
          fname (and (not (eq graph-output 'ascii)) (make-temp-file "metrics-tracker")))

    (with-temp-buffer
      ;; load metric data into bins; list of `bin-data' for each metric in the same order as `metric-names'
      (let* ((bin-data-all (mapcar (lambda (metric-name) (metrics-tracker--bin-metric-data metric-name date-grouping value-transform today))
                                   metric-names))
             ;; prepare the graph data from the bin data
             (data (metrics-tracker--compute-data bin-data-all date-grouping start-date end-date graph-type)))
        (metrics-tracker--make-gnuplot-config metric-names data
                                              date-grouping value-transform
                                              graph-type graph-output fname))
      (save-current-buffer
        (metrics-tracker--setup-output-buffer)
        (fundamental-mode))

      (unless (null fname)
        (setq metrics-tracker-tempfiles (cons fname metrics-tracker-tempfiles)) ; keep track of it so we can delete it
        (add-hook 'kill-emacs-hook #'metrics-tracker-remove-tempfiles))

      (let ((inhibit-read-only t))
        (call-process-region (point-min) (point-max) "gnuplot" nil buffer)
        (set-buffer buffer)
        (goto-char (point-min))
        (if (eq graph-output 'ascii)
            (while (re-search-forward "\f" nil t) ; delete the formfeed in gnuplot output
              (replace-match ""))
          (insert-image (create-image fname) "graph") ; insert the tempfile into the output buffer
          (insert "\n")
          (goto-char (point-min))))

      (metrics-tracker--show-output-buffer))))

(defun metrics-tracker--make-gnuplot-config (metric-names data
                                             date-grouping value-transform
                                             graph-type graph-output fname)
  "Write a gnuplot config (including inline data) to the (empty) current buffer.
METRIC-NAMES a list of metric names being plotted.
DATA the graph data.
DATE-GROUPING the name of the date grouping used to bin the data.
VALUE-TRANSFORM the name of the value transform used on each bin.
GRAPH-TYPE the type of graph (line, bar, scatter).
GRAPH-OUTPUT the graph output format (ascii, svg, png).
FNAME is the filename of the temp file to write."
  (let ((date-format (metrics-tracker--format-bin date-grouping))
        (term (cond ((eq graph-output 'svg) "svg")
                    ((eq graph-output 'png) "pngcairo font \"Arial,10\"")
                    (t "dumb")))
        (width (if (eq graph-output 'ascii) (1- (window-width)) (car metrics-tracker-graph-size)))
        (height (if (eq graph-output 'ascii) (1- (window-height)) (cdr metrics-tracker-graph-size)))
        (title (if (= 1 (length metric-names)) (car metric-names) ""))
        (fg-color (if metrics-tracker-dark-mode "grey50" "grey10"))
        (bg-color (if metrics-tracker-dark-mode "grey10" "grey90")))

    (cond ((eq graph-output 'ascii)
           (insert (format "set term %s size %d, %d\n\n" term width height))
           (insert (format "set title \"%s %s\"\n" title
                           (replace-regexp-in-string "-" " " (symbol-name value-transform)))))
          (t ; output an image
           (insert (format "set term %s size %d, %d background rgb \"%s\"\n\n" term width height bg-color))
           (insert (format "set xtics tc rgb \"%s\"\n" fg-color))
           (insert (format "set title \"%s\" tc rgb \"%s\"\n" title fg-color))
           (insert (format "set output \"%s\"\n" fname))))
    (insert "set tics nomirror\n")
    (insert "set xzeroaxis\n")
    (insert (format "set border 3 back ls -1 lc rgb \"%s\"\n" fg-color))
    (insert (format "set xlabel \"%s\" tc rgb \"%s\"\n" date-grouping fg-color))
    (insert (format "set key tc rgb \"%s\"\n" fg-color))
    (when date-format ; not 'full
      (insert (format "set timefmt \"%s\"\n" date-format))
      (insert (format "set format x \"%s\"\n" date-format)))
    (insert (format "set ylabel \"%s\" tc rgb \"%s\"\n"
                    (replace-regexp-in-string "-" " " (symbol-name value-transform))
                    fg-color))
    (cond ((or (eq graph-type 'line)
               (eq graph-type 'scatter))
           (insert "set xdata time\n")
           (insert (format "set xrange [\"%s\":\"%s\"]\n" (caar data) (caar (last data))))
           (insert "set xtics rotate\n")
           (insert (format "set grid back ls 0 lc \"%s\"\n" fg-color))
           (insert "set pointsize 0.5\n"))
          ((or (eq graph-type 'bar)
               (eq graph-type 'stacked))
           (insert "set xtics rotate\n")
           (insert (format "set boxwidth %f relative\n" (if (eq graph-type 'bar) 1.0 0.6)))
           (insert "set style data histogram\n")
           (insert (format "set style histogram %s\n"
                           (if (eq graph-type 'bar) "cluster" "rowstacked")))
           (insert "set style fill solid\n")
           (insert (format "set grid ytics back ls 0 lc \"%s\"\n" fg-color))
           (if (eq graph-type 'stacked)
               (insert "set key invert\n"))))
    (insert (metrics-tracker--define-plot graph-type graph-output metric-names) "\n")
    (dotimes (_ii (length metric-names))
      (if (not date-format) ; is 'full
          (insert (format ". %s\n" (mapconcat #'identity (cdar data) " ")))
        (dolist (entry data)
          (insert (concat (mapconcat #'identity (nth 1 entry) " ") "\n"))))
      (insert "\ne\n"))))

(defun metrics-tracker--define-plot (graph-type graph-output metric-names)
  "Return the plot definition command.
GRAPH-TYPE is one of line, bar, point
GRAPH-OUTPUT is one of ascii, svg, png
METRIC-NAMES is the list of metric names being plotted."
  (let ((plot-def "plot")
        (colors (nth (if metrics-tracker-dark-mode 1 0) metrics-tracker-graph-colors))
        (num-lines (length metric-names)))
    (dotimes (ii num-lines)
      (let ((dash (if (= 0 ii) "-" ""))
            (label (if (= 1 num-lines) "notitle" (format "title \"%s\"" (nth ii metric-names))))
            (comma (if (< ii (1- num-lines)) "," "")))
        (cond
         ((eq graph-type 'line)
          (setq plot-def (concat plot-def (format " \"%s\" using 1:%d with lines %s lt %s lw 1.2 lc rgbcolor \"%s\"%s"
                                                  dash (+ ii 2) label (1+ ii) (nth ii colors) comma))))
         ((or (eq graph-type 'bar)
              (eq graph-type 'stacked))
          (setq plot-def (concat plot-def (format " \"%s\" using %d:xtic(1) %s lc rgbcolor \"%s\"%s"
                                                  dash (+ ii 2) label (nth ii colors) comma))))
         ((eq graph-type 'scatter)
          (setq plot-def (concat plot-def (format " \"%s\" using 1:%d with points %s lt %d %s lc rgbcolor \"%s\"%s"
                                                  dash (+ ii 2) label
                                                  ; set pointtype to capital letters for ascii or dots for images
                                                  (if (eq graph-output 'ascii) (1+ ii) 7)
                                                  ; but override pointtype to '*' for ascii plots with one metric
                                                  (if (and (eq graph-output 'ascii)
                                                           (= num-lines 1)) "pt \"*\"" "")
                                                  (nth ii colors) comma)))))))
    plot-def))

;;;###autoload
(defun metrics-tracker-save-last-report (name)
"Save the last report as a report named NAME."
  (interactive "sReport name: ")

  ;; verify there is a last report
  (if (null metrics-tracker-last-report-config)
      (error "There is no last report to save"))

  (setq metrics-tracker-named-reports (nconc metrics-tracker-named-reports
                                             (list (cons name metrics-tracker-last-report-config))))
  (message "Saved"))

;;;###autoload
(defun metrics-tracker-show-named-report ()
"Render a previously saved report.
This prompts for which report (saved in
`metrics-tracker-named-reports') to render, then renders it."
  (interactive)

  (if (= (length metrics-tracker-named-reports) 0)
      (error "There are no named reports"))

  (let* ((report-name (completing-read "Named report: " (mapcar #'car metrics-tracker-named-reports) nil t))
         (report (seq-find (lambda (item) (equal (nth 0 item) report-name))
                           metrics-tracker-named-reports))
         (report-type (cadr report))
         (report-config (cddr report)))
    (cond ((eq report-type 'table)
           (metrics-tracker-table-render report-config))
          ((eq report-type 'cal)
           (metrics-tracker-cal-render report-config))
          ((eq report-type 'graph)
           (metrics-tracker-graph-render report-config)))))

(provide 'metrics-tracker)

;;; metrics-tracker.el ends here
