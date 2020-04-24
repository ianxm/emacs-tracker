;; table
(metrics-tracker-table-render '(("commute") full total nil nil)) ; result in status line
(metrics-tracker-table-render '(("commute" "run") full total nil nil)) ; result in table
(metrics-tracker-table-render '(("commute" "run") year total nil nil)) ;
(metrics-tracker-table-render '(("pushups" "ab circuit") month per-week nil nil))
(metrics-tracker-table-render '(("commute") year total "2016-01-01" nil)) ; set start date
(metrics-tracker-table-render '(("commute") month total "2016-06-01" "2019-06-01")) ; set date range
;; failures
(metrics-tracker-table-render '(("fail") fail total nil nil))
(metrics-tracker-table-render '(("commute" "fail") year total nil nil))
(metrics-tracker-table-render '(("commute" "run") fail total nil nil))
(metrics-tracker-table-render '(("commute" "run") year fail nil nil))
(metrics-tracker-table-render '(("commute") month total "2016-01-01" "2015-01-01")) ; reverse date range

;; cal
(metrics-tracker-cal-render '("emacs-tracker" total nil nil))
(metrics-tracker-cal-render '("emacs-tracker" total "2020-04-01" nil))
;; failures
(metrics-tracker-cal-render '("fail" fail nil nil))
(metrics-tracker-cal-render '("emacs-tracker" fail nil nil))

;; graph
(metrics-tracker-graph-render '(("commute" "run") year percent nil nil nil stacked svg))
(metrics-tracker-graph-render '(("commute" "run") year per-week nil nil nil line png))
(metrics-tracker-graph-render '(("pullups" "pushups") month per-week nil nil nil scatter ascii))
(metrics-tracker-graph-render '(("run" "karen run dist") month total "2020-01-01" nil nil bar png))
;; failures
(metrics-tracker-graph-render '(("fail") full total nil nil nil line svg))
(metrics-tracker-graph-render '(("run" "fail") full total nil nil nil line svg))
(metrics-tracker-graph-render '(("commute" "run") fail total nil nil nil line svg))
(metrics-tracker-graph-render '(("commute" "run") full fail nil nil nil line svg))
(metrics-tracker-graph-render '(("commute" "run") full total nil nil nil fail svg))
(metrics-tracker-graph-render '(("commute" "run") full total nil nil nil bar fail))
(metrics-tracker-graph-render '(("commute" "run") full total nil nil nil line svg))
