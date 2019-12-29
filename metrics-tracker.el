;;; metrics-tracker.el --- Generate diagrams of personal metrics from diary entries  -*- lexical-binding: t -*-

;; Copyright (C) 2019 Ian Martins

;; Author: Ian Martins <ianxm@jhu.edu>
;; URL: https://github.com/ianxm/emacs-tracker
;; Version: 0.0.3
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
;; see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; metrics-tracker.el generates tables and charts from the personal metrics
;; data found in your diary entries.

;;; Code:

(require 'seq)
(require 'timezone)
(require 'calendar)

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

(defun metrics-tracker--process-diary (filter action)
  "Read the diary file.
For each valid metrics entry found, parse the fields and then
apply the given FILTER and ACTION."
  (let ((valid-formats '("^\\([[:digit:]\-]+\\) \\([[:ascii:]]+\\) \\([[:digit:]\.]+\\)$"                           ; YYYY-MM-DD
                         "^\\([[:alpha:]]+ [[:digit:]]+, [[:digit:]]+\\) \\([[:ascii:]]+\\) \\([[:digit:]\.]+\\)$"  ; MMM DD, YYYY
                         "^\\([[:digit:]]+ [[:alpha:]]+ [[:digit:]]+\\) \\([[:ascii:]]+\\) \\([[:digit:]\.]+\\)$")) ; DD MMM YYYY
        metric-name metric-date metric-value foundp)
    (with-temp-buffer
      (insert-file-contents diary-file)
      (dolist (line (split-string (buffer-string) "\n" t))
        (setq foundp nil)
        (dolist (format valid-formats)
          (if (string-match format line)
              (setq metric-date (apply #'encode-time (mapcar #'(lambda (x) (or x 0)) ; convert nil to 0
                                                             (seq-take (parse-time-string (match-string 1 line)) 6)))
                    metric-name (intern (match-string 2 line) metrics-tracker-metric-names)
                    metric-value (string-to-number (match-string 3 line))
                    foundp t)))
        (if (and foundp (funcall filter metric-date metric-name))
            (funcall action metric-date metric-name metric-value))))))

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
This reads the diary file and fills in `metrics-tracker-metric-list' if
it is nil.

`metrics-tracker-metric-list' is a list of (metric-name count first last)
sorted by 'last'."
  (unless metrics-tracker-metric-index
    (let* (metrics ; will contain plist of metric-name -> (metric-name count first last since)
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
                               (puthash date-bin (metrics-tracker--val-to-bin value value-transform) bin-data))))
         sorted-bin-data)

    (metrics-tracker--process-diary table-filter-fcn table-action-fcn)

    ;; fill gaps
    (unless (or (eq date-grouping 'full)
                allow-gaps-p)
      (let ((current-date-bin first-date-bin))
        (while (time-less-p current-date-bin today-bin)
          (setq current-date-bin (metrics-tracker--date-to-next-bin current-date-bin date-grouping)) ; increment to next bin
          (unless (gethash current-date-bin bin-data)
            (puthash current-date-bin 0 bin-data)))))

    ;; convert to alist and sort
    (maphash (lambda (key value) (setq sorted-bin-data (cons (cons key value) sorted-bin-data))) bin-data)
    (setq sorted-bin-data (sort sorted-bin-data (lambda (a b) (time-less-p (car a) (car b)))))

    ;; apply value transform (old cdr was count or total, new cdr is requested value transform)
    (dolist (bin sorted-bin-data)
      (setcdr bin (metrics-tracker--bin-to-val (cdr bin) value-transform date-grouping
                                       (car bin) first-date today)))
    sorted-bin-data))

(defun metrics-tracker--setup-output-buffer ()
  "Create and clear the output buffer."
  (let ((buffer (get-buffer-create metrics-tracker-output-buffer-name)))
    (set-buffer buffer)
    (read-only-mode -1)
    (erase-buffer)))

(defun metrics-tracker--show-output-buffer ()
  "Show the output buffer."
  (let ((buffer (get-buffer metrics-tracker-output-buffer-name)))
    (set-buffer buffer)
    (read-only-mode 1)
    (set-window-buffer (selected-window) buffer)))

(defun metrics-tracker--check-gnuplot-exists ()
  "Check if gnuplot is installed on the system."
  (unless (eq 0 (call-process-shell-command "gnuplot --version"))
    (error "Cannot find gnuplot")))

;;;###autoload
(defun metrics-tracker-table ()
  "Get a tabular view of the requested metric."
  (interactive)

  ;; make sure `metrics-tracker-metric-index' has been populated
  (metrics-tracker--load-index)

  (let* ((all-metric-names (mapcar (lambda (metric) (nth 0 metric)) metrics-tracker-metric-index))
         (today (apply #'encode-time (mapcar #'(lambda (x) (or x 0)) ; convert nil to 0
                                             (seq-take (parse-time-string (format-time-string "%F")) 6))))
         ;; ask for params
         (metric-name-str (completing-read "Metric: " all-metric-names nil t))
         (metric-name (intern metric-name-str metrics-tracker-metric-names))
         (date-grouping-str (completing-read "Group dates by: " (metrics-tracker--date-grouping-options) nil t nil nil "month"))
         (date-grouping (intern date-grouping-str))
         (value-transform (intern (completing-read "Value transform: " (metrics-tracker--value-transform-options date-grouping) nil t nil nil "total")))
         ;; load metric data into bins
         (sorted-bin-data (metrics-tracker--bin-metric-data metric-name date-grouping value-transform today)))

    ;; print
    (if (eq date-grouping 'full)
        (message "Overall %s %s: %s"
                 metric-name
                 (replace-regexp-in-string "-" " " (symbol-name value-transform))
                 (cdar sorted-bin-data))

      (metrics-tracker--setup-output-buffer)
      (tabulated-list-mode)

      ;; set headers
      (setq tabulated-list-format (vector (list date-grouping-str 12 t)
                                          (list (format "%s %s" metric-name-str
                                                        (replace-regexp-in-string "-" " " (symbol-name value-transform)))
                                                10
                                                (metrics-tracker--num-sort 1))))
      ;; configure
      (setq tabulated-list-padding 2)
      (setq tabulated-list-sort-key (cons date-grouping-str nil))

      ;; set data
      (let (data date-str)
        (dolist (bin sorted-bin-data)
          (setq date-str (format-time-string (metrics-tracker--format-bin date-grouping) (car bin))
                data (cons (list date-str (vector date-str (cdr bin)))
                           data))
          (setq-local tabulated-list-entries data)))
      (message "entries %s" tabulated-list-entries)

      ;; render the table
      (tabulated-list-init-header)
      (tabulated-list-print nil nil)

      (metrics-tracker--show-output-buffer))))

;;;###autoload
(defun metrics-tracker-cal ()
  "Get a calendar view of the requested metric."
  (interactive)

  ;; make sure `metrics-tracker-metric-index' has been populated
  (metrics-tracker--load-index)

  (let* ((all-metric-names (mapcar (lambda (metric) (nth 0 metric)) metrics-tracker-metric-index))
         (today (apply #'encode-time (mapcar #'(lambda (x) (or x 0)) ; convert nil to 0
                                             (seq-take (parse-time-string (format-time-string "%F")) 6))))
         ;; ask for params
         (metric-name-str (completing-read "Metric: " all-metric-names nil t))
         (metric-name (intern metric-name-str metrics-tracker-metric-names))
         (value-transform (intern (completing-read "Value transform: " (metrics-tracker--value-transform-options 'day) nil t nil nil "total")))
         ;; load metric data into bins
         (sorted-bin-data (metrics-tracker--bin-metric-data metric-name 'day value-transform today t)))

    (metrics-tracker--setup-output-buffer)
    (fundamental-mode)

    ;; render the calendars
    (let* ((first (caar sorted-bin-data))
           (first-decoded (decode-time first))
           (month (nth 4 first-decoded))
           (year (nth 5 first-decoded)))
      (insert (format "  %s\n\n" metric-name-str))
      (put-text-property (point-min) (point-max) 'face 'bold)
      (while sorted-bin-data
        (setq sorted-bin-data (metrics-tracker--print-month month year sorted-bin-data first today))
        (insert "\n\n\n")
        (setq month (1+ month))
        (when (> month 12)
          (setq month 1
                year (1+ year)))))

    (metrics-tracker--show-output-buffer)))

(defun metrics-tracker--print-month (month year sorted-bin-data first today)
  "Write metric data as a calendar for MONTH of YEAR.
SORTED-BIN-DATA contains the data to render.  It ranges from FIRST
until TODAY, which are time values."
  (let ((first-day (encode-time 0 0 0 1 month year))
        day)
    (insert (format "                    %s\n\n" (format-time-string "%b %Y" first-day)))
    (insert "    Su    Mo    Tu    We    Th    Fr    Sa\n  ")
    (dotimes (_ii (nth 6 (decode-time first-day)))
      (insert "      "))
    (dotimes (daynum (metrics-tracker--days-of-month first-day))
      (setq day (encode-time 0 0 0 (1+ daynum) month year))
      (cond ((equal day (caar sorted-bin-data)) (insert (format "%6s" (cdr (pop sorted-bin-data)))))
            ((time-less-p day first) (insert "     _")) ; before data
            ((time-less-p today day) (insert "     _")) ; after today
            (t (insert "     .")))                      ; gap in data
      (when (= (nth 6 (decode-time day)) 6)
          (insert "\n  "))))
  sorted-bin-data)

(defun metrics-tracker--make-gnuplot-config (metric-name
                                     date-grouping value-transform
                                     sorted-bin-data
                                     graph-type graph-output fname)
  "Write a gnuplot config (including inline data) to the (empty) current buffer.
The gnuplot config depends on many variables which must all be
passed in: METRIC-NAME, DATE-GROUPING, VALUE-TRANSFORM,
SORTED-BIN-DATA, GRAPH-TYPE, GRAPH-OUTPUT, FNAME."
  (let ((date-format (metrics-tracker--format-bin date-grouping))
        (term (cond ((eq graph-output 'svg) "svg")
                    ((eq graph-output 'png) "pngcairo")
                    (t "dumb")))
        (width (if (eq graph-output 'ascii) (1- (window-width)) (car metrics-tracker-graph-size)))
        (height (if (eq graph-output 'ascii) (1- (window-height)) (cdr metrics-tracker-graph-size))))
    (cond ((eq graph-output 'ascii)
           (insert (format "set term %s size %d, %d\n\n" term width height))
           (insert (format "set title \"%s %s\"\n"
                           metric-name
                           (replace-regexp-in-string "-" " " (symbol-name value-transform)))))
          (t
           (insert (format "set term %s size %d, %d background rgb \"gray90\"\n\n" term width height))
           (insert (format "set title \"%s\"\n" metric-name))
           (insert (format "set output \"%s\"\n" fname))))
    (insert (format "set xlabel \"%s\"\n" date-grouping))
    (when date-format ; not 'full
      (insert (format "set timefmt \"%s\"\n" date-format))
      (insert (format "set format x \"%s\"\n" date-format)))
    (insert (format "set ylabel \"%s\"\n" value-transform))
    (cond ((eq graph-type 'line)
           (insert "set xdata time\n")
           (insert "set xtics rotate\n")
           (insert "set style line 1 lw 1.2 \n")
           (insert "plot \"-\" using 1:2 with lines notitle ls 1 lc rgbcolor \"#399320\"\n"))
          ((eq graph-type 'bar)
           (insert "set xtics rotate\n")
           (insert "set boxwidth 0.9 relative\n")
           (insert "set yrange [0:]\n")
           (insert "set style data histogram\n")
           (insert "set style histogram cluster\n")
           (insert "set style fill solid\n")
           (insert "plot \"-\" using 2:xtic(1) notitle lc rgbcolor \"#399320\"\n"))
          ((eq graph-type 'scatter)
           (insert "set xdata time\n")
           (insert "set xtics rotate\n")
           (insert "set style line 1 pt 7 ps 0.5\n")
           (insert "plot \"-\" using 1:2 with points notitle ls 1 lc rgbcolor \"#399320\"\n")))
    (if (not date-format)
        (insert (format ". %s\n" (cdar sorted-bin-data)))
      (insert (mapconcat (lambda (x) (format "%s %s"
                                             (format-time-string date-format (car x))
                                             (cdr x)))
                         sorted-bin-data
                         "\n")))
    (insert "\ne")))

;;;###autoload
(defun metrics-tracker-graph ()
  "Get a graph of the requested metric."
  (interactive)

  (metrics-tracker--check-gnuplot-exists)

  ;; make sure `metrics-tracker-metric-index' has been populated
  (metrics-tracker--load-index)

  (let* ((all-metric-names (mapcar (lambda (metric) (nth 0 metric)) metrics-tracker-metric-index))
         (today (apply #'encode-time (mapcar #'(lambda (x) (or x 0)) ; convert nil to 0
                                             (seq-take (parse-time-string (format-time-string "%F")) 6))))
         ;; ask for params
         (metric-name (intern (completing-read "Metric: " all-metric-names nil t) metrics-tracker-metric-names))
         (date-grouping (intern (completing-read "Group dates by: " (metrics-tracker--date-grouping-options) nil t nil nil "month")))
         (value-transform (intern (completing-read "Value transform: " (metrics-tracker--value-transform-options date-grouping) nil t nil nil "total")))
         (graph-type (intern (completing-read "Graph type: " metrics-tracker-graph-options nil t nil nil "line")))
         (graph-output (intern (completing-read "Graph output: " metrics-tracker-graph-output-options nil t nil nil "ascii")))
         ;; load metric data into bins
         (sorted-bin-data (metrics-tracker--bin-metric-data metric-name date-grouping value-transform today))
         ;; prep output buffer
         (buffer (get-buffer-create metrics-tracker-output-buffer-name))
         (fname (and (not (eq graph-output 'ascii)) (make-temp-file "metrics-tracker"))))

    (with-temp-buffer
      (metrics-tracker--make-gnuplot-config metric-name
                                    date-grouping value-transform
                                    sorted-bin-data
                                    graph-type graph-output fname)
      (save-current-buffer
        (metrics-tracker--setup-output-buffer)
        (fundamental-mode))

      (unless (null fname)
        (setq metrics-tracker-tempfiles (cons fname metrics-tracker-tempfiles)) ; keep track of it so we can delete it
        (add-hook 'kill-emacs-hook #'metrics-tracker-remove-tempfiles))
      (call-process-region (point-min) (point-max) "gnuplot" nil buffer)
      (set-buffer buffer)
      (goto-char (point-min))
      (if (eq graph-output 'ascii)
          (while (re-search-forward "\f" nil t) ; delete the formfeed in gnuplot output
            (replace-match ""))
        (insert-image (create-image fname) "graph") ; insert the tempfile into the output buffer
        (insert "\n")
        (goto-char (point-min)))

      (metrics-tracker--show-output-buffer))))

(provide 'metrics-tracker)

;;; metrics-tracker.el ends here
