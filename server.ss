#!/usr/bin/env gxi

(export main)

(import :std/format
	:std/getopt
	:std/iter
	:std/logger
	:std/net/address
	:std/net/httpd
	:std/net/uri
	:std/srfi/19
	:std/text/json
	:std/text/utf8
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
    (http-register-handler httpd "/mattermost" mattermost-handler)
    (thread-join! httpd)))

;; /
(def (departures-handler req res)
  (let/cc return
    (def sncf-key (getenv "SNCF_AUTH_KEY" #f))
    (unless sncf-key
      (errorf "No SNCF API authentication key found. Set the SNCF_AUTH_KEY environment variable.")
      (http-response-write res 500 '(("Content-Type" . "text/plain; charset=utf-8"))
			   "Missing SNCF API authentication key\n")
      (return))
    (def headers (http-request-headers req))
    (def accept-header (assget "Accept" headers))
    (def params (http-request-params req))
    (set! params (if (string? params) (form-url-decode params) '()))
    (def datetime-str (assget "datetime" params))
    (def datetime (if datetime-str
		    (try
		     (string->date datetime-str "~Y~m~dT~H~M~S")
		     (catch _
		       (warnf "Badly formatted date string: ~a" datetime-str)
		       (http-response-write res 400 '(("Content-Type" . "text/plain; charset=utf-8"))
					    (format "Badly formatted date string, expected %Y%m%dT%H%M%S: ~a\n"
						    datetime-str))
		       (return)))
		    #f))
    (def station (assget "station" params))
    (define-values (station-name station-id)
      (if (string? station)
	(get-station-id sncf-key station)
	(values "Vernon - Giverny (Vernon)" "stop_area:SNCF:87415604")))
    (unless station-name
      (set! station-name "Vernon - Giverny (Vernon)"))
    (unless station-id
      (set! station-id "stop_area:SNCF:87415604"))
    (infof "~a ~a ~a ~a (\"~a\")~a"
	   (inet-address->string (http-request-client req))
	   (http-request-method req)
	   (http-request-path req)
	   station-name
	   station
	   (if datetime (string-append " at " datetime-str) ""))
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

;; /mattermost
(def (mattermost-handler req res)
  (let/cc return
    (unless (eq? 'POST (http-request-method req))
      (default-handler req res))
    (def sncf-key (getenv "SNCF_AUTH_KEY" #f))
    (unless sncf-key
      (errorf "No SNCF API authentication key found. Set the SNCF_AUTH_KEY environment variable.")
      (http-response-write res 500 '(("Content-Type" . "text/plain; charset=utf-8"))
			   "Missing SNCF API authentication key\n")
      (return))
    (def raw-req-body (http-request-body req))
    (unless raw-req-body
      (errorf "Empty POST body")
      (http-response-write res 400 '(("Content-Type" . "text/plain; charset=utf-8"))
			   "Empty query\n")
      (return))
    (def req-body (form-url-decode (utf8->string raw-req-body)))
    (def response-url (assget "response_url" req-body))
    (def channel-name (assget "channel_name" req-body))
    (def team-domain (assget "team_domain" req-body))
    (def user-name (assget "user_name" req-body))
    (def text (assget "text" req-body))
    (infof "~a ~a ~a ~a in ~a ~~~a: \"~a\""
	   (inet-address->string (http-request-client req))
	   (http-request-method req)
	   (http-request-path req)
	   user-name
	   team-domain
	   channel-name
	   text)
    (define-values (station-name station-id)
      (if (string? text)
	(get-station-id sncf-key text)
	(values "Vernon - Giverny (Vernon)" "stop_area:SNCF:87415604")))
    (unless station-name
      (set! station-name "Vernon - Giverny (Vernon)"))
    (unless station-id
      (set! station-id "stop_area:SNCF:87415604"))
    (def datetime #f)
    (define-values (departures disruptions) (get-departures sncf-key station-id datetime))
    (def content (with-output-to-string
		   (lambda () (display-all departures disruptions station-name datetime
				      style: 'markdown))))
    (def json-response
      (list->hash-table `(("text" . ,content)
			  ("response_type" . "ephemeral")
			  ("username" . "SCNF Bot")
			  ("icon_url" . ":train:"))))
    (http-response-write res 200 '(("Content-Type" . "application/json"))
			 (json-object->string json-response))))

;; default
(def (default-handler req res)
  (http-response-write res 404 '(("Content-Type" . "text/plain"))
		       "these aren't the droids you are looking for.\n"))
