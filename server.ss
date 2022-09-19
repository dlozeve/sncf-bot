#!/usr/bin/env gxi

(export main)

(import :std/getopt
	:std/iter
	:std/net/httpd
	:std/srfi/19
	:std/sugar
	:gerbil/gambit/threads
	:dlozeve/sncf/api
	:dlozeve/sncf/display)

(def (main . args)
  (def gopt
    (getopt (option 'address "-a" "--address"
                    help: "server address"
                    default: ":8080")))

  (try
   (let (opt (getopt-parse gopt args))
     (run (hash-get opt 'address)))
   (catch (getopt-error? exn)
     (getopt-display-help exn "server" (current-error-port))
     (exit 1))))

(def (run address)
  (let (httpd (start-http-server! address mux: (make-default-http-mux default-handler)))
    (http-register-handler httpd "/" departures-handler)
    (thread-join! httpd)))

;; /
(def (departures-handler req res)
  (def sncf-key (getenv "SNCF_AUTH_KEY" #f))
  (unless sncf-key
    (display "No SNCF API authentication key found. Set the SNCF_AUTH_KEY environment variable.\n"
	     (current-error-port))
    (exit 1))
  (def params (parse-request-params (http-request-params req)))
  (def station (assoc "station" params))
  (when station (set! station (cdr station)))
  (def datetime-str (assoc "datetime" params))
  (def datetime (if datetime-str
		  (string->date (cdr datetime-str) "~Y~m~dT~H~M~S")
		  #f))
  (def style (if (assoc "markdown" params) 'markdown 'unicode))
  (define-values (station-name station-id)
    (if station
      (get-station-id sncf-key station)
      (values "Vernon - Giverny (Vernon)" "stop_area:SNCF:87415604")))
  (define-values (departures disruptions) (get-departures sncf-key station-id datetime))
  (http-response-write res 200 '(("Content-Type" . "text/plain"))
		       (with-output-to-string
			 (lambda () (display-all departures disruptions station-name datetime
					    style: style)))))

(def (parse-request-params params)
  (if params
    (for/collect ((param (string-split params #\&)))
      (match (string-split param #\=)
	([k] (cons k #t))
	([k v] (cons k v))
	([k . rest] (cons k rest))))
    '()))

;; default
(def (default-handler req res)
  (http-response-write res 404 '(("Content-Type" . "text/plain"))
		       "these aren't the droids you are looking for.\n"))
