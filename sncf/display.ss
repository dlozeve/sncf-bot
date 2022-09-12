(export display-departures-table
	display-disruptions)

(import :std/format
	:std/iter
	:std/misc/string
	:std/srfi/19
	:dlozeve/fancy/format
	:dlozeve/fancy/table
	:dlozeve/sncf/api)

(def (compute-table-widths departures)
  (def widths
    (for/collect ((dep departures))
      (with ((departure network direction _ _) dep)
	(list (string-length network) (string-length direction)))))
  (apply map max widths))

(def (display-departures-table departures style: (style 'unicode))
  (def good-emoji (if (eq? style 'markdown) ":white_check_mark: " ""))
  (def bad-emoji (if (eq? style 'markdown) ":warning:          " "[yellow]"))
  (def widths (compute-table-widths departures))
  (def headers (if (eq? style 'markdown)
		 '("Réseau" "Direction" "Heure")
		 '("[bold]Réseau" "[bold]Direction" "[bold]Heure")))
  (def tab (table headers [(map max '(6 9) widths) ... (if (eq? style 'markdown) 32 13)] style))
  (display (table-header tab))
  (for ((dep departures))
    (with ((departure network direction base-dep-dt dep-dt) dep)
      (let* ((hour-str (if (equal? dep-dt base-dep-dt)
			 (str good-emoji (date->string dep-dt "~H:~M"))
			 (str bad-emoji (format "~a → ~a"
						(date->string base-dep-dt "~H:~M")
						(date->string dep-dt "~H:~M"))))))
	(display (table-row tab network direction hour-str)))))
  (display (table-footer tab)))

(def (display-disruptions disruptions style: (style 'unicode))
  (unless (null? disruptions)
    (displayln (if (eq? style 'markdown) "**Perturbations :**" (parse-markup "[bold]Perturbations :"))))
  (for ((dis disruptions))
    (let ((messages (hash-ref dis 'messages '())))
      (display (if (eq? style 'markdown) "* " "• "))
      (displayln (if (null? messages) "[Pas de message]" (hash-ref (car messages) 'text))))))