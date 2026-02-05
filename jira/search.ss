;;; -*- Gerbil -*-
;;; Jira CLI — Search & JQL

(import
  :jira/jira/api
  :jira/jira/config
  :jira/jira/format
  :std/format
  :std/iter
  :std/pregexp
  :std/srfi/13
  :std/sugar
  :std/text/json)

(export #t)

;;;; Search

(def (search-issues config query)
  "Search for issues using JQL or text"
  (let* ((jql (if (or (string-contains query "=")
                      (string-contains query "(")
                      (string-contains query "~"))
               query
               (format "text ~~ '~a'" query)))
         (issues (jira-search config jql))
         (headers ["Key" "Summary" "Status" "Priority" "Assignee" "Updated"])
         (rows []))
    (for (iss issues)
      (when (hash-table? iss)
        (let-hash iss
          (when (hash-table? .?fields)
            (let-hash .fields
              (set! rows (cons [..key
                                (or .?summary "")
                                (if (hash-table? .?status) (hash-get .status 'name) "")
                                (if (hash-table? .?priority) (hash-get .priority 'name) "")
                                (if (hash-table? .?assignee)
                                  (email-short (hash-get .assignee 'emailAddress))
                                  "")
                                (date-short .?updated)]
                               rows)))))))
    (output headers (reverse rows))))

;;;; Saved queries

(def (run-query config alias)
  "Execute a saved query from config"
  (let-hash config
    (if .?queries
      (let ((jql (hash-get .queries alias)))
        (if jql
          (search-issues config jql)
          (begin
            (displayln (format "Error: query '~a' not found in ~/.jira.yaml" alias))
            (exit 2))))
      (begin
        (displayln "Error: no queries defined in ~/.jira.yaml")
        (exit 2)))))

;;;; Saved creations (run)

(def (run-creation config creation-name)
  "Execute a saved creation template from config"
  (let-hash config
    (if .?creations
      (let ((template (hash-get .creations creation-name)))
        (if template
          (begin
            (displayln (format "Running creation template: ~a" creation-name))
            ;; Create the issue from template fields
            (let ((body (jira-post config "/rest/api/3/issue"
                                  (hash ("fields" template)))))
              (when (hash-table? body)
                (displayln (format "Created: ~a" (hash-get body 'key))))))
          (begin
            (displayln (format "Error: creation '~a' not found in ~/.jira.yaml" creation-name))
            (exit 2))))
      (begin
        (displayln "Error: no creations defined in ~/.jira.yaml")
        (exit 2)))))
