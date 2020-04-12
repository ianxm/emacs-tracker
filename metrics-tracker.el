;;; metrics-tracker.el --- Generate diagrams of personal metrics from diary entries  -*- lexical-binding: t -*-

;; Copyright (C) 2019-2020 Ian Martins

;; Author: Ian Martins <ianxm@jhu.edu>
;; URL: https://github.com/ianxm/emacs-tracker
;; Version: 0.1.1
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

;; metrics-tracker.el generates tables and charts from the personal metrics
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

(defcustom metrics-tracker-graph-colors '("#1f77b4" "#ff7f0e" "#2ca02c" "#d62728" "#9467bd" "#8c564b" "#e377c2" "#7f7f7f" "#bcbd22" "#17becf")
  "Colors to use for each series in graphs."
  :type '(list :inline t string)
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

(defconst metrics-tracker-output-buffer-name "*Metrics Tracker Output*"
  "The name of the output buffer.")

(defmacro metrics-tracker--min-date (d1 d2)
  "Return the earlier of the given dates D1 and D2."
  `(if (time-less-p ,d1 ,d2) ,d1 ,d2))

(defmacro metrics-tracker--max-date (d1 d2)
  "Return the later of the given dates D1 and D2."
  `(if (time-less-p ,d1 ,d2) ,d2 ,d1))

(defmacro metrics-tracker--define-plot (num-lines plot-definition)
  "Render the plot definition line to the current buffer.
NUM-LINES is the number of lines to include in the plot.
PLOT-DEFINITION is the gnuplot config command that defines the lines, bars, or points."
  `(progn
    (insert "plot ")
    (dotimes (ii ,num-lines)
      (let ((dash (if (= 0 ii) "-" ""))
            (label (if (= 1 num-lines) "notitle" (format "title \"%s\"" (nth ii metric-names))))
            (comma (if (< ii (1- num-lines)) ", " "")))
        (insert (format ,plot-definition dash (+ ii 2) label (nth ii metrics-tracker-graph-colors) comma))))
    (insert "\n")))

(defun metrics-tracker--process-diary (filter action)
  "Parse the diary file.
For each valid metrics entry found, parse the fields and then
apply the given FILTER and ACTION.

Valid metrics entries look like \"DATE TIME METRICNAME VALUE\" where
- DATE looks like \"2020-01-01\" or \"Jan 1, 2020\" or \"1 Jan 2020\"
- TIME (which we ignore) looks like \"10:30\" or \"10:30a\" or \"10:30 am\"
- METRICNAME is any string, whitespace included
- VALUE is a decimal number like \"1\" or \"1.2\""

  (let ((valid-formats '("^\\([[:digit:]]\\{4\\}\-[[:digit:]]\\{2\\}\-[[:digit:]]\\{2\\}\\) *\\(?:[[:digit:]\:]+ ?[ap]?m?\\)? *\\([[:ascii:]]+\\) \\([[:digit:]\.]+\\)$"     ; YYYY-MM-DD
                         "^\\([[:digit:]]\\{2\\}\/[[:alpha:]]+\/[[:digit:]]\\{4\\}\\) *\\(?:[[:digit:]\:]+ ?[ap]?m?\\)? *\\([[:ascii:]]+\\) \\([[:digit:]\.]+\\)$"           ; DD/MMM/YYYY
                         "^\\([[:alpha:]]+ [[:digit:]]\\{1,2\\}, [[:digit:]]\\{4\\}\\) *\\(?:[[:digit:]\:]\\{1,8\\} ?[ap]?m?\\)? *\\([[:ascii:]]+\\) \\([[:digit:]\.]+\\)$"  ; MMM DD, YYYY
                         "^\\([[:digit:]]\\{1,2\\} [[:alpha:]]+ [[:digit:]]\\{4\\}\\) *\\(?:[[:digit:]\:]\\{1,8\\} ?[ap]?m?\\)? *\\([[:ascii:]]+\\) \\([[:digit:]\.]+\\)$")) ; DD MMM YYYY
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
                          metric-value (string-to-number (match-string 3 line))
                          foundp t)))
              (if (and foundp (funcall filter metric-date metric-name))
                  (funcall action metric-date metric-name metric-value)))
            (error (message "error parsing line: %s" line)))))))

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
           (today (apply #'encode-time (mapcar #'(lambda (x) (or x 0)) ; convert nil to 0
                                               (seq-take (parse-time-string (format-time-string "%F")) 6))))
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

  ;; set data
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

(defvar metrics-tracker-grouping-and-transform-options
  '(day (total count)
        week (total count percent per-day)
        month (total count percent per-day per-week)
        year (total count percent per-day per-week per-month)
        full (total count percent per-day per-week per-month per-year))
  "This is a plist of date-grouping options mapped to value-transform options.")

(defun metrics-tracker--date-grouping-options ()
  "Pull the list of date-grouping options out of `metrics-tracker-grouping-and-transform-options'."
  (seq-filter (lambda (x) (symbolp x)) metrics-tracker-grouping-and-transform-options))

(defun metrics-tracker--value-transform-options (date-grouping)
  "Look up the valid value-transforms for the given DATE-GROUPING."
  (plist-get metrics-tracker-grouping-and-transform-options date-grouping))

(defvar metrics-tracker-graph-options '(line bar scatter)
  "The types of supported graphs.")

(defvar metrics-tracker-graph-output-options '(ascii svg png)
  "The graph output options.")

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
   ((eq value-transform 'percent) 1)
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
     ((eq value-transform 'total) (format "%g" value))
     ((eq value-transform 'count) (format "%d" value))
     ((eq value-transform 'percent) (format "%.1f" (* (/ value bin-duration) 100)))
     ((eq value-transform 'per-day) (format "%.1f" (* value (/ 1 bin-duration))))
     ((eq value-transform 'per-week) (format "%.1f" (* value (/ 7 bin-duration))))
     ((eq value-transform 'per-month) (format "%.1f" (* value (/ 30 bin-duration))))
     ((eq value-transform 'per-year) (format "%.1f" (* value (/ 365 bin-duration)))))))

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

    ;; fill gaps
    (unless (or (eq date-grouping 'full)
                allow-gaps-p)
      (let ((current-date-bin first-date-bin))
        (while (time-less-p current-date-bin today-bin)
          (setq current-date-bin (metrics-tracker--date-to-next-bin current-date-bin date-grouping)) ; increment to next bin
          (unless (gethash current-date-bin bin-data)
            (puthash current-date-bin 0 bin-data)))))

    ;; apply the value transform to the hashtable values
    (maphash (lambda (date bin)
               (puthash date
                        (metrics-tracker--bin-to-val bin value-transform date-grouping
                                                          date first-date today)
                        bin-data))
             bin-data)
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

;;;###autoload
(defun metrics-tracker-table (arg)
  "Get a tabular view of the requested metric.
If ARG is given, allow selection of multiple metrics."
  (interactive "P")

  ;; make sure `metrics-tracker-metric-index' has been populated
  (metrics-tracker--load-index)

  (let* ((ivy-sort-functions-alist nil)
         (today (apply #'encode-time (mapcar #'(lambda (x) (or x 0)) ; convert nil to 0
                                             (seq-take (parse-time-string (format-time-string "%F")) 6))))
         ;; ask for params
         (multp (not (null arg)))
         (metric-names-str (metrics-tracker--ask-for-metrics multp))
         (metric-names (mapcar (lambda (name) (intern name metrics-tracker-metric-names)) metric-names-str))
         (date-grouping-str (completing-read "Group dates by: " (metrics-tracker--presorted-options
                                                                 (metrics-tracker--date-grouping-options))
                                             nil t nil nil "month"))
         (date-grouping (intern date-grouping-str))
         (value-transform (intern (completing-read "Value transform: " (metrics-tracker--presorted-options
                                                                        (metrics-tracker--value-transform-options date-grouping))
                                                   nil t nil nil "total")))
         ;; load metric data into bins; list of `bin-data' for each metric in the same order as `metric-names'
         (bin-data-all (mapcar (lambda (metric-name) (metrics-tracker--bin-metric-data metric-name date-grouping value-transform today))
                               metric-names))
         merged-dates)

    ;; merge dates and sort
    (setq merged-dates (seq-reduce (lambda (dates ii) (append (hash-table-keys ii) dates))
                                   bin-data-all
                                   nil))
    (delete-dups merged-dates)
    (setq merged-dates (metrics-tracker--sort-dates merged-dates))

    (if (and (eq date-grouping 'full)
             (not multp))
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
            (vconcat (list (list date-grouping-str 12 t))
                     (mapcar (lambda (metric-name)
                               (let ((label (format "%s %s" metric-name (replace-regexp-in-string "-" " " (symbol-name value-transform)))))
                                 (list label (max 10 (1+ (length label))) (metrics-tracker--num-sort 1))))
                             metric-names-str)))

      ;; configure
      (setq tabulated-list-padding 2)
      (setq tabulated-list-sort-key (cons date-grouping-str nil))

      ;; set table data
      (let (data date-str)
        (dolist (date merged-dates)
          (setq date-str (if (eq date-grouping 'full)
                             "full"
                           (format-time-string (metrics-tracker--format-bin date-grouping) date)))
          (setq data (cons (list date-str
                                 (vconcat (list date-str)
                                          (mapcar (lambda (bin-data) (gethash date bin-data "")) bin-data-all)))
                           data))
          (setq-local tabulated-list-entries data)))

      ;; render the table
      (let ((inhibit-read-only t))
        (tabulated-list-init-header)
        (tabulated-list-print nil nil))

      (metrics-tracker--show-output-buffer))))

;;;###autoload
(defun metrics-tracker-cal ()
  "Get a calendar view of the requested metric."
  (interactive)

  ;; make sure `metrics-tracker-metric-index' has been populated
  (metrics-tracker--load-index)

  (let* ((ivy-sort-functions-alist nil)
         (all-metric-names (mapcar (lambda (metric) (nth 0 metric)) metrics-tracker-metric-index))
         (today (apply #'encode-time (mapcar #'(lambda (x) (or x 0)) ; convert nil to 0
                                             (seq-take (parse-time-string (format-time-string "%F")) 6))))
         ;; ask for params
         (metric-name-str (completing-read "Metric: " (metrics-tracker--presorted-options all-metric-names) nil t))
         (metric-name (intern metric-name-str metrics-tracker-metric-names))
         (value-transform (intern (completing-read "Value transform: " (metrics-tracker--presorted-options
                                                                        (metrics-tracker--value-transform-options 'day))
                                                   nil t nil nil "total")))
         ;; load metric data into bins
         (bin-data (metrics-tracker--bin-metric-data metric-name 'day value-transform today t))
         ;; find the first date
         (dates (hash-table-keys bin-data))
         (first (seq-reduce (lambda (first ii) (if (time-less-p first ii) first ii))
                                    dates (car dates))))

    (metrics-tracker--setup-output-buffer)
    (fundamental-mode)

    ;; render the calendars
    (let* ((inhibit-read-only t)
           (first-decoded (decode-time first))
           (month (nth 4 first-decoded))
           (year (nth 5 first-decoded))
           (today-decoded (decode-time today))
           (this-month (nth 4 today-decoded))
           (this-year (nth 5 today-decoded)))
      (insert (format "  %s\n\n" metric-name-str))
      (put-text-property (point-min) (point-max) 'face 'bold)
      (while (or (<= month this-month)
                 (< year this-year))
        (metrics-tracker--print-month month year bin-data first today)
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

(defun metrics-tracker--make-gnuplot-config (metric-names
                                     date-grouping value-transform
                                     bin-data-all
                                     graph-type graph-output fname)
  "Write a gnuplot config (including inline data) to the (empty) current buffer.
METRIC-NAMES a list of metric names being plotted.
DATE-GROUPING the name of the date grouping used to bin the data.
VALUE-TRANSFORM the name of the value transform used on each bin.
BIN-DATA-ALL a list bin-data (which is a hash of dates to values) in the same order as METRIC-NAMEs.
GRAPH-TYPE the type of graph (line, bar, scatter).
GRAPH-OUTPUT the graph output format (ascii, svg, png).
FNAME is the filename of the temp file to write."
  (let (merged-dates data num-lines
        (date-format (metrics-tracker--format-bin date-grouping))
        (term (cond ((eq graph-output 'svg) "svg")
                    ((eq graph-output 'png) "pngcairo")
                    (t "dumb")))
        (width (if (eq graph-output 'ascii) (1- (window-width)) (car metrics-tracker-graph-size)))
        (height (if (eq graph-output 'ascii) (1- (window-height)) (cdr metrics-tracker-graph-size)))
        (title (if (= 1 (length metric-names)) (car metric-names) "")))

    ;; merge dates and sort
    (setq merged-dates (seq-reduce (lambda (dates ii) (append (hash-table-keys ii) dates))
                                   bin-data-all
                                   nil))
    (delete-dups merged-dates)
    (setq merged-dates (metrics-tracker--sort-dates merged-dates))

    ;; set table data
    (let (date-str)
      (dolist (date merged-dates)
        (setq date-str (if (eq date-grouping 'full)
                           "full"
                         (format-time-string (metrics-tracker--format-bin date-grouping) date)))
        (setq data (cons (append (list date-str)
                                 (mapcar (lambda (bin-data) (gethash date bin-data ".")) bin-data-all))
                         data)))
      (setq data (reverse data))
      (setq num-lines (1- (length (car data)))))

    (cond ((eq graph-output 'ascii)
           (insert (format "set term %s size %d, %d\n\n" term width height))
           (insert (format "set title \"%s %s\"\n" title
                           (replace-regexp-in-string "-" " " (symbol-name value-transform)))))
          (t
           (insert (format "set term %s size %d, %d background rgb \"gray90\"\n\n" term width height))
           (insert (format "set title \"%s\"\n" title))
           (insert (format "set output \"%s\"\n" fname))))
    (insert (format "set xlabel \"%s\"\n" date-grouping))
    (when date-format ; not 'full
      (insert (format "set timefmt \"%s\"\n" date-format))
      (insert (format "set format x \"%s\"\n" date-format)))
    (insert (format "set ylabel \"%s\"\n" value-transform))
    (cond ((eq graph-type 'line)
           (insert "set xdata time\n")
           (insert "set xtics rotate\n")
           (metrics-tracker--define-plot
            num-lines
            "\"%s\" using 1:%d with lines %s lt 1 lw 1.2 lc rgbcolor \"%s\"%s"))
          ((eq graph-type 'bar)
           (insert "set xtics rotate\n")
           (insert "set boxwidth 0.9 relative\n")
           (insert "set yrange [0:]\n")
           (insert "set style data histogram\n")
           (insert "set style histogram cluster\n")
           (insert "set style fill solid\n")
           (metrics-tracker--define-plot
            num-lines
            "\"%s\" using %d:xtic(1) %s lc rgbcolor \"%s\"%s"))
          ((eq graph-type 'scatter)
           (insert "set xdata time\n")
           (insert "set xtics rotate\n")
           (insert "set style line 1 pt 7 ps 0.5\n")
           (metrics-tracker--define-plot
            num-lines
            "\"%s\" using 1:%d with points %s lc rgbcolor \"%s\"%s")))
    (dotimes (_ii num-lines)
      (if (not date-format)
          (insert (format ". %s\n" (cdar data)))
        (dolist (entry data)
          (insert (concat (mapconcat #'identity entry " ") "\n"))))
      (insert "\ne\n"))))

;;;###autoload
(defun metrics-tracker-graph (arg)
  "Get a graph of the requested metric.
If ARG is given, allow selection of multiple metrics."
  (interactive "P")

  (metrics-tracker--check-gnuplot-exists)

  ;; make sure `metrics-tracker-metric-index' has been populated
  (metrics-tracker--load-index)

  (let* ((ivy-sort-functions-alist nil)
         (today (apply #'encode-time (mapcar #'(lambda (x) (or x 0)) ; convert nil to 0
                                             (seq-take (parse-time-string (format-time-string "%F")) 6))))
         ;; ask for params
         (multp (not (null arg)))
         (metric-names-str (metrics-tracker--ask-for-metrics multp))
         (metric-names (mapcar (lambda (name) (intern name metrics-tracker-metric-names)) metric-names-str))
         (date-grouping (intern (completing-read "Group dates by: " (metrics-tracker--presorted-options
                                                                     (metrics-tracker--date-grouping-options))
                                                 nil t nil nil "month")))
         (value-transform (intern (completing-read "Value transform: " (metrics-tracker--presorted-options
                                                                        (metrics-tracker--value-transform-options date-grouping))
                                                   nil t nil nil "total")))
         (graph-type (intern (completing-read "Graph type: " (metrics-tracker--presorted-options metrics-tracker-graph-options)
                                              nil t nil nil "line")))
         (graph-output (intern (completing-read "Graph output: " (metrics-tracker--presorted-options
                                                                  metrics-tracker-graph-output-options)
                                                nil t nil nil "ascii")))
         ;; load metric data into bins; list of `bin-data' for each metric in the same order as `metric-names'
         (bin-data-all (mapcar (lambda (metric-name) (metrics-tracker--bin-metric-data metric-name date-grouping value-transform today))
                               metric-names))

         ;; prep output buffer
         (buffer (get-buffer-create metrics-tracker-output-buffer-name))
         (fname (and (not (eq graph-output 'ascii)) (make-temp-file "metrics-tracker"))))

    (with-temp-buffer
      (metrics-tracker--make-gnuplot-config metric-names
                                    date-grouping value-transform
                                    bin-data-all
                                    graph-type graph-output fname)
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

(provide 'metrics-tracker)

;;; metrics-tracker.el ends here
