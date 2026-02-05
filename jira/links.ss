;;; -*- Gerbil -*-
;;; Jira CLI — Issue Links & Remote Links

(import
  :jira/jira/api
  :jira/jira/format
  :std/format
  :std/iter
  :std/sugar
  :std/text/json)

(export #t)

;;;; Issue Links

(def (link-create config link-type inward-issue outward-issue)
  "Create an issue link"
  (jira-post config "/rest/api/3/issueLink"
             (hash ("type" (hash ("name" link-type)))
                   ("inwardIssue" (hash ("key" inward-issue)))
                   ("outwardIssue" (hash ("key" outward-issue)))))
  (displayln (format "Created link '~a' from ~a to ~a" link-type inward-issue outward-issue)))

(def (link-get config link-id)
  "Get issue link details"
  (let ((body (jira-get config (format "/rest/api/3/issueLink/~a" link-id))))
    (when (hash-table? body)
      (let-hash body
        (display-item "Link ID" .?id)
        (display-item "Type" (if (hash-table? .?type) (hash-get .type 'name) ""))
        (display-item "Inward" (if (hash-table? .?inwardIssue) (hash-get .inwardIssue 'key) ""))
        (display-item "Outward" (if (hash-table? .?outwardIssue) (hash-get .outwardIssue 'key) ""))))))

(def (link-delete config link-id)
  "Delete an issue link"
  (jira-delete config (format "/rest/api/3/issueLink/~a" link-id))
  (displayln (format "Deleted link ~a" link-id)))

(def (link-types config)
  "List issue link types"
  (let ((body (jira-get config "/rest/api/3/issueLinkType"))
        (headers ["Id" "Name" "Inward" "Outward"])
        (rows []))
    (when (hash-table? body)
      (let-hash body
        (when .?issueLinkTypes
          (for (lt .issueLinkTypes)
            (when (hash-table? lt)
              (let-hash lt
                (set! rows (cons [(or .?id "") (or .?name "") (or .?inward "") (or .?outward "")]
                                 rows))))))))
    (output headers (reverse rows))))

;;;; Remote Links

(def (remote-link-list config issue)
  "List remote links on an issue"
  (let ((body (jira-get config (format "/rest/api/3/issue/~a/remotelink" issue)))
        (headers ["Id" "Title" "URL" "Relationship"])
        (rows []))
    (when (list? body)
      (for (link body)
        (when (hash-table? link)
          (let-hash link
            (let ((link-url (if (hash-table? .?object) (hash-get .object 'url) ""))
                  (link-title (if (hash-table? .?object) (hash-get .object 'title) "")))
              (set! rows (cons [(or .?id "") link-title link-url (or .?relationship "")]
                               rows)))))))
    (output headers (reverse rows))))

(def (remote-link-create config issue url title)
  "Create a remote link on an issue"
  (jira-post config (format "/rest/api/3/issue/~a/remotelink" issue)
             (hash ("object" (hash ("url" url) ("title" title)))))
  (displayln (format "Created remote link '~a' on ~a" title issue)))

(def (remote-link-delete config issue link-id)
  "Delete a remote link from an issue"
  (jira-delete config (format "/rest/api/3/issue/~a/remotelink/~a" issue link-id))
  (displayln (format "Deleted remote link ~a from ~a" link-id issue)))
