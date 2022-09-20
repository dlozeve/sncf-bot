#!/usr/bin/env gxi

(export main)

(import :std/format
	:std/getopt
	:std/iter
	:std/logger
	:std/net/address
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
     (parameterize ((current-logger-options 'DEBUG))
       (start-logger!)
       (run (hash-get opt 'address))))
   (catch (getopt-error? exn)
     (getopt-display-help exn "server" (current-error-port))
     (exit 1))))

(def (run address)
  (infof "Starting HTTP server on ~a" (inet-address->string address))
  (let (httpd (start-http-server! address mux: (make-default-http-mux default-handler)))
    (http-register-handler httpd "/" departures-handler)
    (thread-join! httpd)))

;; /
(def (departures-handler req res)
  (let/cc return
    (infof "~a ~a ~a"
	   (http-request-method req)
	   (http-request-path req)
	   (inet-address->string (http-request-client req)))
    (def sncf-key (getenv "SNCF_AUTH_KEY" #f))
    (unless sncf-key
      (errorf "No SNCF API authentication key found. Set the SNCF_AUTH_KEY environment variable.")
      (http-response-write res 500 '(("Content-Type" . "text/plain; charset=utf-8"))
			   "Missing SNCF API authentication key\n")
      (return))
    (def headers (http-request-headers req))
    (def accept-header (assoc "Accept" headers))
    (when accept-header (set! accept-header (cdr accept-header)))
    (def params (parse-request-params (http-request-params req)))
    (def station (assoc "station" params))
    (when station (set! station (cdr station)))
    (def datetime-str (assoc "datetime" params))
    (def datetime (if datetime-str
		    (try
		     (string->date (cdr datetime-str) "~Y~m~dT~H~M~S")
		     (catch _
		       (warnf "Badly formatted date string: ~a" (cdr datetime-str))
		       (http-response-write res 400 '(("Content-Type" . "text/plain; charset=utf-8"))
					    (format "Badly formatted date string, expected %Y%m%dT%H%M%S: ~a\n"
						    (cdr datetime-str)))
		       (return)))
		    #f))
    (define-values (station-name station-id)
      (if (string? station)
	(get-station-id sncf-key station)
	(values "Vernon - Giverny (Vernon)" "stop_area:SNCF:87415604")))
    (define-values (departures disruptions) (get-departures sncf-key station-id datetime))
    (def content
      (cond
       ((or (assoc "markdown" params) (string-prefix? "text/markdown" accept-header))
	(with-output-to-string
	  (lambda () (display-all departures disruptions station-name datetime
			     style: 'markdown))))
       ((string-prefix? "text/html" accept-header) ; TODO
	(with-output-to-string
	  (lambda () (display-all departures disruptions station-name datetime
			     style: 'markdown))))
       (#t
	(with-output-to-string
	  (lambda () (display-all departures disruptions station-name datetime
			     style: 'unicode))))))
    (http-response-write res 200 '(("Content-Type" . "text/plain; charset=utf-8"))
			 content)))

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
