#!/usr/bin/env gxi

(export main)

(import :std/getopt
	:std/srfi/19
	:std/sugar
	:dlozeve/sncf/api
	:dlozeve/sncf/display
	:dlozeve/sncf/mattermost)

(def (main . args)
  (define-values (station datetime mattermost-url mattermost-channel) (parse-arguments args))
  (def sncf-key (getenv "SNCF_AUTH_KEY" #f))
  (unless sncf-key
    (display "No SNCF API authentication key found. Set the SNCF_AUTH_KEY environment variable.\n"
	     (current-error-port))
    (exit 1))
  (define-values (station-name station-id)
    (if station
      (get-station-id sncf-key station)
      (values "Vernon - Giverny (Vernon)" "stop_area:SNCF:87415604")))
  (define-values (departures disruptions) (get-departures sncf-key station-id datetime))
  (display-all departures disruptions station-name datetime)
  (when mattermost-url
    (let ((tab-str-md
	   (with-output-to-string
	     (lambda () (display-all departures disruptions station-name datetime style: 'markdown)))))
      (post-to-mattermost mattermost-url tab-str-md channel: mattermost-channel))))

(def (parse-arguments args)
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
     (values station datetime mattermost-url mattermost-channel))
   (catch (getopt-error? exn)
     (getopt-display-help exn "sncf" (current-error-port))
     (exit 1))
   (catch (exn)
     (display (error-message exn) (current-error-port))
     (exit 1))))
