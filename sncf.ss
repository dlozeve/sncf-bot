#!/usr/bin/env gxi

(export main)

(import :std/format
	:std/getopt
	:std/iter
	:std/misc/string
	:std/net/request
	:std/srfi/19
	:std/sugar
	:std/text/json
	:dlozeve/fancy/table)

(def +sncf-url+ "https://api.sncf.com/v1/coverage/sncf/stop_areas/stop_area:SNCF:87415604/departures")

(def (main . args)
  (def gopt (getopt (argument 'sncf-key help: "SNCF API authentication key.")
		    (option 'mattermost-url "--mattermost-url" help: "Mattermost incoming webhook URL.")
		    (option 'mattermost-channel "--channel" help: "Mattermost channel.")))
  (try
   (let* ((options (getopt-parse gopt args))
	  (sncf-key (hash-ref options 'sncf-key))
	  (mattermost-url (hash-ref options 'mattermost-url))
	  (mattermost-channel (hash-ref options 'mattermost-channel)))
     (let-values (((departures disruptions) (get-departures sncf-key)))
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

(def (get-departures sncf-key)
  (def req (http-get +sncf-url+ headers: `(("Authorization" . ,sncf-key))))
  (if (eq? 200 (request-status req))
    (let (req-json (request-json req))
      (values (hash-ref req-json 'departures)
	      (hash-ref req-json 'disruptions)))
    (begin (display (format "Failed to query the SNCF API: ~a ~a\n"
			    (request-status req) (request-status-text req)))
	   (exit 1))))

(def (display-departures-table departures style: (style 'unicode))
  (def good-emoji (if (eq? style 'markdown) ":white_check_mark: " ""))
  (def bad-emoji (if (eq? style 'markdown) ":warning:          " ""))
  (def tab (table '("Réseau" "Direction" "Heure") [10 30 (if (eq? style 'markdown) 32 13)] style))
  (display (table-header tab))
  (for ((dep departures))
    (let* ((info (hash-ref dep (string->symbol "display_informations")))
	   (stop-dt (hash-ref dep (string->symbol "stop_date_time")))
	   (base-dep-dt-str (hash-ref stop-dt (string->symbol "base_departure_date_time")))
	   (base-dep-dt (string->date base-dep-dt-str "~Y~m~dT~H~M~S"))
	   (dep-dt-str (hash-ref stop-dt (string->symbol "departure_date_time")))
	   (dep-dt (string->date dep-dt-str "~Y~m~dT~H~M~S"))
	   (hour-str (if (equal? dep-dt base-dep-dt)
		       (str good-emoji (date->string dep-dt "~H:~M"))
		       (str bad-emoji (format "~a → ~a"
					      (date->string base-dep-dt "~H:~M")
					      (date->string dep-dt "~H:~M"))))))
      (display (table-row tab
			  (hash-ref info 'network)
			  (hash-ref info 'direction)
			  hour-str))))
  (display (table-footer tab)))

(def (display-disruptions disruptions style: (style 'unicode))
  (displayln "Perturbations :")
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
