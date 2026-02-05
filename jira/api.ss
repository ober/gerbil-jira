;;; -*- Gerbil -*-
;;; Jira CLI — HTTP API Layer

(import
  :std/format
  :std/net/request
  :std/net/uri
  :std/sugar
  :std/text/json)

(export #t)

;;;; HTTP helpers

(def (default-headers basic-auth)
  [["Accept" . "application/json"]
   ["Content-Type" . "application/json"]
   ["Authorization" . basic-auth]])

(def (check-response resp)
  "Check HTTP response status. Return parsed JSON body or raise error."
  (let ((status (request-status resp)))
    (if (and (>= status 200) (< status 300))
      (let ((text (request-text resp)))
        (if (and text (> (string-length text) 0))
          (parameterize ((read-json-key-as-symbol? #t))
            (with-input-from-string text read-json))
          #t))
      (let ((body (request-text resp)))
        (error (format "HTTP ~a: ~a" status (or body "")))))))

;;;; Core request functions

(def (jira-get config path)
  (let-hash config
    (let ((url (format "~a~a" .url path)))
      (check-response
       (http-get url headers: (default-headers .basic-auth))))))

(def (jira-post config path data)
  (let-hash config
    (let ((url (format "~a~a" .url path)))
      (check-response
       (http-post url
                  headers: (default-headers .basic-auth)
                  data: (if (string? data) data (json-object->string data)))))))

(def (jira-put config path data)
  (let-hash config
    (let ((url (format "~a~a" .url path)))
      (check-response
       (http-put url
                 headers: (default-headers .basic-auth)
                 data: (if (string? data) data (json-object->string data)))))))

(def (jira-delete config path)
  (let-hash config
    (let ((url (format "~a~a" .url path)))
      (check-response
       (http-delete url headers: (default-headers .basic-auth))))))

;;;; Pagination helper

(def (jira-get-paginated config path
       results-key: (results-key 'values)
       max-results: (max-results 100))
  "Auto-paginate through Jira API results. Returns complete list."
  (let loop ((offset 0) (acc []))
    (let* ((sep (if (string-contains path "?") "&" "?"))
           (full-path (format "~a~astartAt=~a&maxResults=~a" path sep offset max-results))
           (body (jira-get config full-path)))
      (if (hash-table? body)
        (let ((items (hash-get body results-key))
              (total (hash-get body 'total)))
          (if (and items (list? items) (> (length items) 0))
            (let ((new-acc (append acc items)))
              (if (and total (> total (length new-acc)))
                (loop (+ offset (length items)) new-acc)
                new-acc))
            acc))
        acc))))

;;;; Search helper (different pagination pattern)

(def (jira-search config jql
       fields: (fields #f)
       max-results: (max-results 100))
  "Execute JQL search with automatic pagination."
  (let loop ((offset 0) (acc []))
    (let* ((encoded-jql (uri-encode jql))
           (field-param (if fields (format "&fields=~a" (string-join fields ",")) ""))
           (path (format "/rest/api/3/search/jql?jql=~a&maxResults=~a&startAt=~a~a"
                         encoded-jql max-results offset field-param))
           (body (jira-get config path)))
      (if (hash-table? body)
        (let-hash body
          (let ((issues (or .?issues [])))
            (if (> (length issues) 0)
              (let ((new-acc (append acc issues)))
                (if (and .?total (> .total (length new-acc)))
                  (loop (+ offset (length issues)) new-acc)
                  new-acc))
              acc)))
        acc))))
