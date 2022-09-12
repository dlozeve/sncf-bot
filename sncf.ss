#!/usr/bin/env gxi

(export main)

(import :std/format
	:std/getopt
	:std/srfi/19
	:std/sugar
	:dlozeve/fancy/format
	:dlozeve/sncf/api
	:dlozeve/sncf/display
	:dlozeve/sncf/mattermost)

(def (main . args)
  (def gopt (getopt (optional-argument 'station help: "Name of the station (default Vernon-Giverny).")
		    (optional-argument 'datetime help: "Date and time (ISO 8601 format).")
		    (flag 'help "-h" "--help" help: "Display this help.")
		    (option 'mattermost-url "--mattermost-url" help: "Mattermost incoming webhook URL.")
		    (option 'mattermost-channel "--channel" help: "Mattermost channel.")))
  (try
   (let* ((options (getopt-parse gopt args))
	  (station (hash-ref options 'station))
	  (datetime-str (hash-ref options 'datetime))
	  (datetime (if datetime-str (string->date datetime-str "~Y-~m-~dT~H:~M:~S") #f))
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
     (let-values (((departures disruptions) (get-departures sncf-key station-id datetime)))
       (display (parse-markup
		 (format "[bold]Prochains départs de [green]~a[/green] " station-name)))
       (displayln (if datetime
		    (parse-markup (format "le ~a à ~a :"
					  (date->string datetime "~a ~d ~b ~Y")
					  (date->string datetime "~H:~M")))
		    ":"))
       (display-departures-table departures)
       (display-disruptions disruptions)
       (when mattermost-url
	 (let ((tab-str-md (with-output-to-string
			     (lambda ()
			       (display (format "Prochains départs de **~a** " station-name))
			       (displayln (if datetime
					    (parse-markup (format "le ~a à ~a :\n"
								  (date->string datetime "~a ~d ~b ~Y")
								  (date->string datetime "~H:~M")))
					    ":\n"))
			       (display-departures-table departures style: 'markdown)
			       (display-disruptions disruptions style: 'markdown)))))
	   (post-to-mattermost mattermost-url tab-str-md channel: mattermost-channel)))))
   (catch (getopt-error? exn)
     (getopt-display-help exn "sncf" (current-error-port))
     (exit 1))
   (catch (exn)
     (display (error-message exn) (current-error-port))
     (exit 1))))
