;; table
(metrics-tracker-table-render '(("commute") full total)) ; result in status line
(metrics-tracker-table-render '(("commute" "run") full total)) ; result in table
(metrics-tracker-table-render '(("commute" "run") year total)) ;
(metrics-tracker-table-render '(("pushups" "ab circuit") month per-week))
;; failures
(metrics-tracker-table-render '(("fail") fail total))
(metrics-tracker-table-render '(("commute" "fail") year total))
(metrics-tracker-table-render '(("commute" "run") fail total))
(metrics-tracker-table-render '(("commute" "run") year fail))

;; cal
(metrics-tracker-cal-render '("commute" total))
;; failures
(metrics-tracker-cal-render '("fail" fail))
(metrics-tracker-cal-render '("commute" fail))

;; graph
(metrics-tracker-graph-render '(("commute" "run") year percent nil nil nil stacked svg))
(metrics-tracker-graph-render '(("commute" "run") year per-week nil nil nil line png))
(metrics-tracker-graph-render '(("pullups" "pushups") month per-week nil nil nil scatter ascii))
;; failures
(metrics-tracker-graph-render '(("fail") full total nil nil nil line svg))
(metrics-tracker-graph-render '(("run" "fail") full total nil nil nil line svg))
(metrics-tracker-graph-render '(("commute" "run") fail total nil nil nil line svg))
(metrics-tracker-graph-render '(("commute" "run") full fail nil nil nil line svg))
(metrics-tracker-graph-render '(("commute" "run") full total nil nil nil fail svg))
(metrics-tracker-graph-render '(("commute" "run") full total nil nil nil bar fail))
(metrics-tracker-graph-render '(("commute" "run") full total nil nil nil line svg))
