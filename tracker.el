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

;; tracker.el generates tables and charts from the personal metrics
;; data found in your diary entries.

;;; Code:

(require 'seq)
(require 'timezone)

(defcustom tracker-graph-size '(700 . 500)
  "Specifies the size as (width . height) to be used for graph images."
  :type '(cons integer integer)
  :group 'tracker)

(defcustom tracker-metric-name-whitelist nil
  "A list of metric names to include in reports.  If this is
specified, only the metrics in this list are considered.  All
others are filtered out.  If this is set, then
`tracker-metric-name-blacklist' has no effect.

For example: '(\"pushups\" \"situps\")"
  :type '(list :inline t string)
  :group 'tracker)

(defcustom tracker-metric-name-blacklist nil
  "A list of metric names to exclude from reports.  This is
ignored if `tracker-metric-name-whitelist' is set.

For example: '(\"pushups\" \"situps\")"
  :type '(list :inline t string)
  :group 'tracker)

(defvar tracker-metric-index nil
  "This is the list of metrics read from the diary file.
It is a list containing: (name count first last) for each metric.
It is cleared when the tracker output buffer is killed, forcing
the diary file to be re-read if the data is needed again.")

(defvar tracker-tempfiles nil
  "This is the list of tempfiles (graph images) that have been created during the current session.")

(defvar tracker-metric-names (make-vector 5 0)
  "This is an obarray of all existing metric names.")

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
  "Clear cached data and delete tempfiles.
Clear the data cached in `tracker-metric-index' in order to force
it to be re-read from the diary file the next time it is
needed.  Also delete the tempfiles (graph images) listed in
`tracker-tempfiles'."
  (when (string= (buffer-name (current-buffer)) "*Tracker Output*")
    (setq tracker-metric-index nil)
    (tracker-remove-tempfiles)
    (remove-hook 'kill-buffer-hook #'tracker-clear-data)))

(defun tracker-remove-tempfiles ()
  "Remove any tempfiles (graph images) that were created during the current session."
  (dolist (elt tracker-tempfiles)
    (if (file-exists-p elt)
        (delete-file elt)))
  (setq tracker-tempfiles nil)
  (remove-hook 'kill-emacs-hook #'tracker-remove-tempfiles))

(defun tracker--load-index ()
  "Make sure the metric index has been populated.
This reads the diary file and fills in `tracker-metric-list' if
it is nil."
  (when (not tracker-metric-index)
    (let* (metrics ; will contain plist of metric-name -> (metric-name count first last)
           existing-metric
           (list-filter-fcn (cond ((not (null tracker-metric-name-whitelist))
                                   (lambda (_date name) ; filter out non-whitelisted metrics
                                     (seq-contains tracker-metric-name-whitelist (symbol-name name))))
                                  ((not (null tracker-metric-name-blacklist))
                                   (lambda (_date name) ; filter out blacklisted metrics
                                     (not (seq-contains tracker-metric-name-blacklist (symbol-name name)))))
                                  (t                    ; keep all metrics
                                   (lambda (_date _name) t))))
           (list-action-fcn (lambda (date name _value)
                              (setq existing-metric (plist-get metrics name))
                              (if (not existing-metric)
                                  (setq metrics (plist-put metrics
                                                           name
                                                           (list name 1 date date)))
                                (setcar (nthcdr 1 existing-metric) (1+ (nth 1 existing-metric)))
                                (setcar (nthcdr 2 existing-metric) (tracker--min-date (nth 2 existing-metric) date))
                                (setcar (nthcdr 3 existing-metric) (tracker--max-date (nth 3 existing-metric) date))))))

      ;; read the diary file, fill `metrics' plist with "name -> (name count first last)"
      (tracker--process-diary list-filter-fcn list-action-fcn)

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

;;;###autoload
(defun tracker-list ()
  "Display a list of all saved metrics in the output buffer.
This reads the diary file."
  (interactive)

  (tracker--load-index)

  (tracker--setup-output-buffer)

  (insert "| metric | count | first | last | days |\n") ; header
  (insert "|--\n")
  (dolist (metric tracker-metric-index)
    (insert (format "| %s | %s | %s | %s | %s |\n"      ; data
                    (nth 0 metric)
                    (nth 1 metric)
                    (format-time-string "%F" (nth 2 metric))
                    (format-time-string "%F" (nth 3 metric))
                    (1+ (- (time-to-days (nth 3 metric))
                           (time-to-days (nth 2 metric)))))))
  (goto-char (point-min))
  (orgtbl-mode t)
  (org-ctrl-c-ctrl-c)
  (tracker--show-output-buffer))

(defvar tracker-grouping-and-transform-options
  '(day (total count)
    week (total count percent per-day)
    month (total count percent per-day per-week)
    year (total count percent per-day per-week per-month)
    full (total count percent per-day per-week per-month per-year))
  "This is a plist of date-grouping options mapped to value-transform options.")

(defun tracker--date-grouping-options ()
  "Pull the list of date-grouping options out of `tracker-grouping-and-transform-options'."
  (seq-filter (lambda (x) (symbolp x)) tracker-grouping-and-transform-options))

(defun tracker--value-transform-options (date-grouping)
  "Look up the valid value-transforms for the given DATE-GROUPING."
  (plist-get tracker-grouping-and-transform-options date-grouping))

(defvar tracker-graph-options '(line bar scatter)
  "The types of supported graphs.")

(defvar tracker-graph-output-options '(ascii svg png)
  "The graph output options.")

(defun tracker--date-to-bin (date date-grouping)
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

(defun tracker--date-to-next-bin (date date-grouping)
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
  "Convert the VALUE to be stored for the bin based on VALUE-TRANSFORM.
Either save the total value or a count of occurrences."
  (cond
   ((eq value-transform 'count) 1)
   ((eq value-transform 'percent) 1)
   (t value)))

(defun tracker--format-bin (date-grouping)
  "Get the format string for the the bin based on the DATE-GROUPING."
  (cond
       ((eq date-grouping 'day) "%Y-%m-%d")
       ((eq date-grouping 'week) "%Y-%m-%d")
       ((eq date-grouping 'month) "%Y-%m")
       ((eq date-grouping 'year) "%Y")))

(defun tracker--trim-duration (span bin-start first-date last-date)
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

(defun tracker--days-of-month (date)
  "Find the number of days in the month containing DATE.  This depends on `timezeone'."
  (let ((date-fields (decode-time date)))
    (timezone-last-day-of-month (nth 4 date-fields) (nth 5 date-fields))))

(defun tracker--bin-to-val (value
                            value-transform date-grouping
                            bin-date first-date today)
  "Transform and format the bin VALUE into the value used in reporting.

The VALUE-TRANSFORM and DATE-GROUPING are needed to determine how
to transform the value.  BIN-DATE, FIRST-DATE, TODAY are all
needed to determine the number of days in the current bin."
  (let ((bin-duration (cond
                       ((eq date-grouping 'day) 1.0)
                       ((eq date-grouping 'week) (tracker--trim-duration 7 bin-date first-date today))
                       ((eq date-grouping 'month) (tracker--trim-duration (tracker--days-of-month bin-date) bin-date first-date today))
                       ((eq date-grouping 'year) (tracker--trim-duration 365 bin-date first-date today))
                       ((eq date-grouping 'full) (float (- (time-to-days today)
                                                           (time-to-days first-date)))))))
    (cond
     ((eq value-transform 'total) (format "%g" value))
     ((eq value-transform 'count) value)
     ((eq value-transform 'percent) (format "%.1f" (* (/ value bin-duration) 100)))
     ((eq value-transform 'per-day) (format "%.1f" (* value (/ 1 bin-duration))))
     ((eq value-transform 'per-week) (format "%.1f" (* value (/ 7 bin-duration))))
     ((eq value-transform 'per-month) (format "%.1f" (* value (/ 30 bin-duration))))
     ((eq value-transform 'per-year) (format "%.1f" (* value (/ 365 bin-duration)))))))

(defun tracker--bin-metric-data (metric-name date-grouping value-transform today)
  "Read the requested metric data from the diary.
Only keep entries for METRIC-NAME.  Apply DATE-GROUPING and
VALUE-TRANSFORM.  Fill gaps ending at TODAY.  Return the sorted
bin data as (list (date . pretransformed-value))."
  (let* ((bin-data (make-hash-table :test 'equal))
         (first-date (nth 2 (nth 0 (seq-filter (lambda (item) (eq (car item) metric-name)) tracker-metric-index))))
         (first-date-bin (tracker--date-to-bin first-date date-grouping))
         (today-bin (tracker--date-to-bin today date-grouping))
         date-bin found-value
         (table-filter-fcn (lambda (_date name)
                             (eq name metric-name)))
         (table-action-fcn (lambda (date _name value)
                             (setq date-bin (tracker--date-to-bin date date-grouping)
                                   found-value (gethash date-bin bin-data))
                             (if found-value
                                 (puthash date-bin (+ found-value (tracker--val-to-bin value value-transform)) bin-data)
                               (puthash date-bin (tracker--val-to-bin value value-transform) bin-data))))
         sorted-bin-data)

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

    ;; apply value transform (old cdr was count or total, new cdr is requested value transform)
    (dolist (bin sorted-bin-data)
      (setcdr bin (tracker--bin-to-val (cdr bin) value-transform date-grouping
                                       (car bin) first-date today)))
    sorted-bin-data))

(defun tracker--setup-output-buffer ()
  "Create and clear the output buffer."
  (let ((buffer (get-buffer-create "*Tracker Output*")))
      (set-buffer buffer)
      (read-only-mode -1)
      (erase-buffer)))

(defun tracker--show-output-buffer ()
  "Show the output buffer."
  (let ((buffer (get-buffer "*Tracker Output*")))
    (set-buffer buffer)
    (read-only-mode 1)
    (set-window-buffer (selected-window) buffer))
  )

(defun tracker--check-gnuplot-exists ()
  "Check if gnuplot is installed on the system."
  (unless (eq 0 (call-process-shell-command "gnuplot --version"))
    (error "Cannot find gnuplot")))

;;;###autoload
(defun tracker-table ()
   "Get a tabular view of the requested metric."
  (interactive)

  ;; make sure `tracker-metric-index' has been populated
  (tracker--load-index)

  (let* ((all-metric-names (mapcar (lambda (metric) (nth 0 metric)) tracker-metric-index))
         (today (tracker--string-to-date (format-time-string "%F")))
         ;; ask for params
         (metric-name (intern (completing-read "Metric: " all-metric-names nil t) tracker-metric-names))
         (date-grouping (intern (completing-read "Group dates by: " (tracker--date-grouping-options) nil t nil nil "month")))
         (value-transform (intern (completing-read "Value transform: " (tracker--value-transform-options date-grouping) nil t nil nil "total")))
         ;; load metric data into bins
         (sorted-bin-data (tracker--bin-metric-data metric-name date-grouping value-transform today)))

    ;; print
    (if (eq date-grouping 'full)
        (message "Overall %s %s: %s"
                 metric-name
                 (replace-regexp-in-string "-" " " (symbol-name value-transform))
                 (cdar sorted-bin-data))

      (tracker--setup-output-buffer)

      (set-buffer "*Tracker Output*")
      (insert (format "| %s | %s %s |\n" ; header
                      date-grouping
                      metric-name
                      (replace-regexp-in-string "-" " " (symbol-name value-transform))))
      (insert "|--\n")
      (dolist (bin sorted-bin-data)
        (insert (format "| %s | %s |\n"  ; data
                        (format-time-string (tracker--format-bin date-grouping) (car bin))
                        (cdr bin))))
      (goto-char (point-min))
      (orgtbl-mode t)
      (org-ctrl-c-ctrl-c)

      (tracker--show-output-buffer))))


(defun tracker--make-gnuplot-config (metric-name
                                     date-grouping value-transform
                                     sorted-bin-data
                                     graph-type graph-output fname)
  "Write a gnuplot config (including inline data) to the (empty) current buffer.
The gnuplot config depends on many variables which must all be
passed in: METRIC-NAME, DATE-GROUPING, VALUE-TRANSFORM,
SORTED-BIN-DATA, GRAPH-TYPE, GRAPH-OUTPUT, FNAME."
  (let ((date-format (tracker--format-bin date-grouping))
        (term (cond ((eq graph-output 'svg) "svg")
                    ((eq graph-output 'png) "pngcairo")
                    (t "dumb")))
        (width (if (eq graph-output 'ascii) (1- (window-width)) (car tracker-graph-size)))
        (height (if (eq graph-output 'ascii) (1- (window-height)) (cdr tracker-graph-size))))
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
    (insert (format "set timefmt \"%s\"\n" date-format))
    (insert (format "set format x \"%s\"\n" date-format))
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
    (insert (mapconcat (lambda (x) (format "%s %s"
                                           (format-time-string date-format (car x))
                                           (cdr x)))
                       sorted-bin-data
                       "\n"))
    (insert "\ne")))

;;;###autoload
(defun tracker-graph ()
  "Get a graph of the requested metric."
  (interactive)

  (tracker--check-gnuplot-exists)

  ;; make sure `tracker-metric-index' has been populated
  (tracker--load-index)

  (let* ((all-metric-names (mapcar (lambda (metric) (nth 0 metric)) tracker-metric-index))
         (today (tracker--string-to-date (format-time-string "%F")))
         ;; ask for params
         (metric-name (intern (completing-read "Metric: " all-metric-names nil t) tracker-metric-names))
         (date-grouping (intern (completing-read "Group dates by: " (tracker--date-grouping-options) nil t nil nil "month")))
         (value-transform (intern (completing-read "Value transform: " (tracker--value-transform-options date-grouping) nil t nil nil "total")))
         (graph-type (intern (completing-read "Graph type: " tracker-graph-options nil t nil nil "line")))
         (graph-output (intern (completing-read "Graph output: " tracker-graph-output-options nil t nil nil "ascii")))
         ;; load metric data into bins
         (sorted-bin-data (tracker--bin-metric-data metric-name date-grouping value-transform today))
         ;; prep output buffer
         (buffer (get-buffer-create "*Tracker Output*"))
         (fname (and (not (eq graph-output 'ascii)) (make-temp-file "tracker"))))

    (with-temp-buffer
      (tracker--make-gnuplot-config metric-name
                                    date-grouping value-transform
                                    sorted-bin-data
                                    graph-type graph-output fname)
      (save-current-buffer
        (tracker--setup-output-buffer))

      (unless (null fname)
        (setq tracker-tempfiles (cons fname tracker-tempfiles)) ; keep track of it so we can delete it
        (add-hook 'kill-emacs-hook #'tracker-remove-tempfiles))
      (call-process-region (point-min) (point-max) "gnuplot" nil buffer)
      (set-buffer buffer)
      (goto-char (point-min))
      (if (eq graph-output 'ascii)
          (while (re-search-forward "\f" nil t) ; delete the formfeed in gnuplot output
            (replace-match ""))
        (insert-image (create-image fname) "graph") ; insert the tempfile into the output buffer
        (insert "\n")
        (goto-char (point-min)))

      (tracker--show-output-buffer))))

(provide 'tracker)

;;; tracker.el ends here
