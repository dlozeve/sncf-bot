(export get-station-id
	(struct-out departure)
	get-departures)

(import :std/format
	:std/iter
	:std/logger
	:std/net/request
	:std/net/uri
	:std/srfi/19
	:std/text/json)

(def +sncf-url+ "https://api.sncf.com/v1/coverage/sncf")

(def (get-station-id sncf-key station)
  (def url (format "~a/pt_objects?q=~a&type\\[\\]=stop_area" +sncf-url+ (uri-encode station)))
  (def req (http-get url headers: `(("Authorization" . ,sncf-key))))
  (if (eq? 200 (request-status req))
    (let (req-json (request-json req))
      (let ((pts (hash-ref req-json 'pt_objects #f)))
	(if (and pts (not (null? pts)))
	  (values (hash-ref (car pts) 'name) (hash-ref (car pts) 'id))
	  (begin (warnf  "No station found for \"~a\"." station)
		 (values #f #f)))))
    (begin (warnf "Failed to query the SNCF API: ~a ~a"
		  (request-status req) (request-status-text req))
	   (values #f #f))))

(def (get-departures sncf-key station-id (datetime #f))
  (def url (format "~a/stop_areas/~a/departures" +sncf-url+ station-id))
  (def params (if datetime
		`(("from_datetime" . ,(date->string datetime "~Y~m~dT~H~M~S")))
		#f))
  (def req (http-get url headers: `(("Authorization" . ,sncf-key)) params: params))
  (if (eq? 200 (request-status req))
    (let (req-json (request-json req))
      (values (parse-departures (hash-ref req-json 'departures))
	      (hash-ref req-json 'disruptions)))
    (begin (warnf "Failed to query the SNCF API: ~a ~a"
		  (request-status req) (request-status-text req))
	   (values #f #f))))

(defstruct departure (network direction base-datetime datetime))

(def (parse-departures departures-json)
  (for/collect ((dep departures-json))
    (let* ((info (hash-ref dep (string->symbol "display_informations")))
	   (stop-dt (hash-ref dep (string->symbol "stop_date_time")))
	   (base-dep-dt-str (hash-ref stop-dt (string->symbol "base_departure_date_time")))
	   (base-dep-dt (string->date base-dep-dt-str "~Y~m~dT~H~M~S"))
	   (dep-dt-str (hash-ref stop-dt (string->symbol "departure_date_time")))
	   (dep-dt (string->date dep-dt-str "~Y~m~dT~H~M~S")))
      (departure (hash-ref info 'network)
		 (hash-ref info 'direction)
		 base-dep-dt
		 dep-dt))))
