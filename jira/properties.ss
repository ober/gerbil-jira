;;; -*- Gerbil -*-
;;; Jira CLI — Entity Properties

(import
  :jira/jira/api
  :jira/jira/format
  :std/format
  :std/iter
  :std/sugar
  :std/text/json)

(export #t)

;;;; Issue Properties

(def (property-list config issue)
  "List properties on an issue"
  (let ((body (jira-get config (format "/rest/api/3/issue/~a/properties" issue)))
        (headers ["Key" "Self"])
        (rows []))
    (when (hash-table? body)
      (let-hash body
        (when (and .?keys (list? .keys))
          (for (prop .keys)
            (when (hash-table? prop)
              (let-hash prop
                (set! rows (cons [(or .?key "") (or .?self "")] rows))))))))
    (output headers (reverse rows))))

(def (property-get config issue key)
  "Get a specific property value"
  (let ((body (jira-get config (format "/rest/api/3/issue/~a/properties/~a" issue key))))
    (when (hash-table? body)
      (let-hash body
        (display-item "Key" .?key)
        (display-item "Value" (if (hash-table? .?value)
                                (json-object->string .value)
                                .?value))))))

(def (property-set config issue key value)
  "Set a property value on an issue"
  (jira-put config (format "/rest/api/3/issue/~a/properties/~a" issue key)
            (if (string? value)
              (hash ("value" value))
              value))
  (displayln (format "Set property '~a' on ~a" key issue)))

(def (property-delete config issue key)
  "Delete a property from an issue"
  (jira-delete config (format "/rest/api/3/issue/~a/properties/~a" issue key))
  (displayln (format "Deleted property '~a' from ~a" key issue)))
