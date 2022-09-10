#!/usr/bin/env gxi

(export main)

(import :std/format
	:std/getopt
	:std/iter
	:std/misc/string
	:std/net/request
	:std/net/uri
	:std/srfi/19
	:std/sugar
	:std/text/json
	:dlozeve/fancy/table)

(def +sncf-url+ "https://api.sncf.com/v1/coverage/sncf")

(def (main . args)
  (def gopt (getopt (optional-argument 'station help: "Name of the station (default Vernon-Giverny).")
		    (flag 'help "-h" "--help" help: "Display this help.")
		    (option 'mattermost-url "--mattermost-url" help: "Mattermost incoming webhook URL.")
		    (option 'mattermost-channel "--channel" help: "Mattermost channel.")))
  (try
   (let* ((options (getopt-parse gopt args))
	  (station (hash-ref options 'station))
	  (help (hash-ref options 'help #f))
	  (mattermost-url (hash-ref options 'mattermost-url))
	  (mattermost-channel (hash-ref options 'mattermost-channel)))
     (when help
       (getopt-display-help gopt "sncf")
       (exit 0))
     (def sncf-key (getenv "SNCF_AUTH_KEY" #f))
     (unless sncf-key
       (display "No SNCF API authentication key found. Set the SNCF_AUTH_KEY environment variable.\n"
		(current-error-port))
       (exit 1))
     (define-values (station-name station-id)
       (if station
	 (get-station-id sncf-key station)
	 (values "Vernon - Giverny (Vernon)" "stop_area:SNCF:87415604")))
     (let-values (((departures disruptions) (get-departures sncf-key station-id)))
       (displayln (str "Station : " station-name))
       (display-departures-table departures)
       (display-disruptions disruptions)
       (when mattermost-url
	 (let ((tab-str-md (with-output-to-string (lambda ()
						    (display-departures-table departures style: 'markdown)
						    (display-disruptions disruptions style: 'markdown)))))
	   (post-to-mattermost mattermost-url tab-str-md channel: mattermost-channel)))))
   (catch (getopt-error? exn)
     (getopt-display-help exn "sncf" (current-error-port))
     (exit 1))
   (catch (exn)
     (display (error-message exn) (current-error-port))
     (exit 1))))

(def (get-station-id sncf-key station)
  (def url (format "~a/pt_objects?q=~a&type\\[\\]=stop_area" +sncf-url+ (uri-encode station)))
  (def req (http-get url headers: `(("Authorization" . ,sncf-key))))
  (if (eq? 200 (request-status req))
    (let (req-json (request-json req))
      (let ((pts (hash-ref req-json 'pt_objects #f)))
	(if (and pts (not (null? pts)))
	  (values (hash-ref (car pts) 'name) (hash-ref (car pts) 'id))
	  (begin (display (format "No station found for \"~a\".\n" station) (current-error-port))
		 (exit 1)))))
    (begin (display (format "Failed to query the SNCF API: ~a ~a\n"
			    (request-status req) (request-status-text req)))
	   (exit 1))))

(def (get-departures sncf-key station-id)
  (def url (format "~a/stop_areas/~a/departures" +sncf-url+ station-id))
  (def req (http-get url headers: `(("Authorization" . ,sncf-key))))
  (if (eq? 200 (request-status req))
    (let (req-json (request-json req))
      (values (parse-departures (hash-ref req-json 'departures))
	      (hash-ref req-json 'disruptions)))
    (begin (display (format "Failed to query the SNCF API: ~a ~a\n"
			    (request-status req) (request-status-text req)))
	   (exit 1))))

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

(def (display-departures-table departures style: (style 'unicode))
  (def good-emoji (if (eq? style 'markdown) ":white_check_mark: " ""))
  (def bad-emoji (if (eq? style 'markdown) ":warning:          " ""))
  (def tab (table '("Réseau" "Direction" "Heure") [15 60 (if (eq? style 'markdown) 32 13)] style))
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
    (displayln "Perturbations :"))
  (for ((dis disruptions))
    (if (eq? style 'markdown)
      (display "* ")
      (display "• "))
    (displayln (hash-ref (car (hash-ref dis 'messages)) 'text))))

(def (post-to-mattermost url text channel: (channel #f))
  (def data (list->hash-table `((text . ,text))))
  (when channel
    (hash-put! data 'channel channel))
  (def mattermost-headers '(("Content-Type" . "application/json")))
  (def req (http-post url
		      headers: mattermost-headers
		      data: (json-object->string data)))
  (if (eq? 200 (request-status req))
    (display (format "Sent to Mattermost in ~a\n" (if channel channel "default channel")))
    (begin (display (format "Failed to send the message to Mattermost: ~a ~a\n"
			    (request-status req) (request-status-text req)))
	   (exit 1))))
