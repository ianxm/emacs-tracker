;; table
(metrics-tracker-table-render '(("commute") full total nil nil)) ; result in status line
(metrics-tracker-table-render '(("commute" "run dist") full total nil nil)) ; full result in table
(metrics-tracker-table-render '(("commute" "run dist") year total nil nil)) ; by year
(metrics-tracker-table-render '(("pushups" "ab circuit") month per-week nil nil)) ; by month, per-week transform
(metrics-tracker-table-render '(("commute") year total "2016-01-01" nil)) ; set start date
(metrics-tracker-table-render '(("commute") month total "2016-06-01" "2019-06-01")) ; set date range
;; failures
(metrics-tracker-table-render '(("fail") fail total nil nil))
(metrics-tracker-table-render '(("commute" "fail") year total nil nil))
(metrics-tracker-table-render '(("commute" "run dist") fail total nil nil))
(metrics-tracker-table-render '(("commute" "run dist") year fail nil nil))
(metrics-tracker-table-render '(("commute") month total "2016-01-01" "2015-01-01")) ; reverse date range

;; graph
(metrics-tracker-graph-render '(("commute" "run dist") year percent nil nil nil stacked png))
(metrics-tracker-graph-render '(("commute") year percent nil nil nil bar svg))
(metrics-tracker-graph-render '(("commute" "run dist") year per-week nil nil nil line png))
(metrics-tracker-graph-render '(("commute" "run dist") week total nil nil nil scatter png))
(metrics-tracker-graph-render '(("pullups" "pushups") month per-week nil nil nil scatter ascii))
(metrics-tracker-graph-render '(("run dist" "karen run dist") month total "2020-01-01" nil nil bar png))
;; failures
(metrics-tracker-graph-render '(("fail") full total nil nil nil line svg))
(metrics-tracker-graph-render '(("run dist" "fail") full total nil nil nil line svg))
(metrics-tracker-graph-render '(("commute" "run dist") fail total nil nil nil line svg))
(metrics-tracker-graph-render '(("commute" "run dist") full fail nil nil nil line svg))
(metrics-tracker-graph-render '(("commute" "run dist") full total nil nil nil fail svg))
(metrics-tracker-graph-render '(("commute" "run dist") full total nil nil nil bar fail))
(metrics-tracker-graph-render '(("commute" "run dist") full total nil nil nil line svg))

;; cal
(metrics-tracker-cal-render '(("emacs-tracker") total nil nil nil))
(metrics-tracker-cal-render '(("emacs-tracker") total "2020-04-01" nil nil))
;; failures
(metrics-tracker-cal-render '(("fail") fail nil nil nil))
(metrics-tracker-cal-render '(("emacs-tracker") fail nil nil nil))

;; math expressions test
(metrics-tracker-table-render '(("karen run dist" "karen run time") week total nil nil "dist=$1, mph=$1/$2")) ; two metrics to one expression
(metrics-tracker-graph-render '(("karen run dist" "karen run time") week total nil nil "dist (mi)=$1, time (h)=$2, mph=$1/$2" line svg)) ; two metrics to three expressions
(metrics-tracker-graph-render '(("karen run dist" "karen run time") week total nil nil "mph=$1/$2" line svg)) ; two metrics to one expression
(metrics-tracker-cal-render '(("karen run dist" "karen run time") total nil nil "mph=$1/$2")) ; cal with expression

;; readme reports
(metrics-tracker-table-render '(("beer") year percent nil nil nil))
(metrics-tracker-cal-render '(("emacs-tracker") total "2019-12-01" "2019-12-30"))
(metrics-tracker-graph-render '(("commute") year per-week nil nil nil line ascii))
(metrics-tracker-graph-render '(("commute") week total nil nil nil scatter png))
(metrics-tracker-graph-render '(("commute" "run dist") year percent nil nil nil stacked png)) ; dark mode
(metrics-tracker-graph-render '(("beer") month total nil nil nil line png)) ; dark mode
(metrics-tracker-table-render '(("karen run dist" "karen run time") month total nil nil "dist (mi)=$1, time (h)=$2, mph=$1/$2")) ; blank on divide by zero
(metrics-tracker-graph-render '(("karen run dist" "karen run time") week total nil nil "mph=$1/$2, goal=4.5" line png)) ; two metrics to one expression
