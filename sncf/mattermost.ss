(export post-to-mattermost)

(import :std/format
	:std/net/request
	:std/text/json)

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
