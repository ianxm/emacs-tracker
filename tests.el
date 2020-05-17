;; table
(metrics-tracker-table-render '(("commute") full total nil nil)) ; result in status line
(metrics-tracker-table-render '(("commute" "ian elliptical dist") full total nil nil)) ; full result in table
(metrics-tracker-table-render '(("commute" "ian elliptical dist") year total nil nil)) ; by year
(metrics-tracker-table-render '(("pushups" "ab circuit") month per-week nil nil)) ; by month, per-week transform
(metrics-tracker-table-render '(("commute") year total "2016-01-01" nil)) ; set start date
(metrics-tracker-table-render '(("commute") month total "2016-06-01" "2019-06-01")) ; set date range
;; failures
(metrics-tracker-table-render '(("fail") fail total nil nil))
(metrics-tracker-table-render '(("commute" "fail") year total nil nil))
(metrics-tracker-table-render '(("commute" "ian elliptical dist") fail total nil nil))
(metrics-tracker-table-render '(("commute" "ian elliptical dist") year fail nil nil))
(metrics-tracker-table-render '(("commute") month total "2016-01-01" "2015-01-01")) ; reverse date range

;; graph
(metrics-tracker-graph-render '(("commute" "ian elliptical dist") year percent nil nil stacked png))
(metrics-tracker-graph-render '(("commute") year percent nil nil bar svg))
(metrics-tracker-graph-render '(("commute" "ian elliptical dist") year per-week nil nil line png))
(metrics-tracker-graph-render '(("commute" "ian elliptical dist") week total nil nil scatter png))
(metrics-tracker-graph-render '(("pullups" "pushups") month per-week nil nil scatter ascii))
(metrics-tracker-graph-render '(("ian elliptical dist" "karen elliptical dist") month total "2020-01-01" nil bar png))
(metrics-tracker-graph-render '(("ian elliptical mph" "karen elliptical mph") week avg "2020-01-01" nil line png))
;; failures
(metrics-tracker-graph-render '(("fail") full total nil nil line svg))
(metrics-tracker-graph-render '(("ian elliptical dist" "fail") full total nil nil line svg))
(metrics-tracker-graph-render '(("commute" "ian elliptical dist") fail total nil nil line svg))
(metrics-tracker-graph-render '(("commute" "ian elliptical dist") full fail nil nil line svg))
(metrics-tracker-graph-render '(("commute" "ian elliptical dist") full total nil nil fail svg))
(metrics-tracker-graph-render '(("commute" "ian elliptical dist") full total nil nil bar fail))
(metrics-tracker-graph-render '(("commute" "ian elliptical dist") full total nil nil line svg))

;; cal
(metrics-tracker-cal-render '("emacs-tracker" total nil nil))
(metrics-tracker-cal-render '("emacs-tracker" total "2020-04-01" nil))
;; failures
(metrics-tracker-cal-render '("fail" total nil nil))
(metrics-tracker-cal-render '("emacs-tracker" fail nil nil))

;; derived metrics
(metrics-tracker-table-render '(("elisp coding") year total nil nil)) ; total
(metrics-tracker-table-render '(("elisp coding") year per-month nil nil)) ; per-month
(metrics-tracker-table-render '(("elisp coding") year count nil nil)) ; count
(metrics-tracker-table-render '(("coding") year total nil nil)) ; total
(metrics-tracker-table-render '(("coding") year per-day nil nil)) ; per-day
(metrics-tracker-table-render '(("coding") year count nil nil)) ; count
(metrics-tracker-table-render '(("karen elliptical mph") full avg nil nil)) ; full avg
(metrics-tracker-table-render '(("karen elliptical dist" "karen elliptical mph") week avg nil nil)) ; base and derived
(metrics-tracker-table-render '(("ian elliptical mph" "karen elliptical mph") week avg "2019-12-01" nil)) ; base and derived range limited
(metrics-tracker-table-render '(("abs") year per-month nil nil)) ; per-month
(metrics-tracker-table-render '(("abs") year count nil nil)) ; count
(metrics-tracker-graph-render '(("cardio work" "workout") year percent nil nil line svg)) ; total
(metrics-tracker-graph-render '(("calisthenics" "cardio work") month percent nil nil line svg)) ; percent
(metrics-tracker-graph-render '(("calisthenics" "cardio work") month count nil nil line svg)) ; count
(metrics-tracker-graph-render '(("coding") month percent nil nil line svg)) ; double derived
(metrics-tracker-graph-render '(("coding" "elisp coding") month percent nil nil line svg)) ; double derived

(metrics-tracker-table-render '(("karen elliptical mph") month max nil nil)) ; derived max
(metrics-tracker-table-render '(("karen elliptical mph") month avg nil nil)) ; derived avg
(metrics-tracker-cal-render '("karen elliptical mph" total nil nil)) ; cal derived
;; test cycle error

;; readme reports
(metrics-tracker-graph-render '(("drinks" "workout" "coding") year percent nil nil bar svg))
(metrics-tracker-graph-render '(("drinks") month total nil nil line png))
(metrics-tracker-cal-render '("emacs-tracker" total nil "2019-12-30"))
(metrics-tracker-graph-render '(("commute") year per-week nil nil line ascii))

;; gallery
(metrics-tracker-graph-render '(("commute" "ian elliptical dist") week total nil nil scatter png))
(metrics-tracker-graph-render '(("commute") month total nil nil line png))
(metrics-tracker-graph-render '(("commute" "ian elliptical dist") year percent nil nil stacked png))
(metrics-tracker-table-render '(("karen elliptical dist" "karen elliptical mph") week avg nil nil))

(customize-group "metrics-tracker")
