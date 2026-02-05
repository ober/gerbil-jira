;;; -*- Gerbil -*-
;;; Jira CLI — Issues, Comments, Votes, Watchers, Worklogs, Attachments

(import
  :ober/jira/api
  :ober/jira/config
  :ober/jira/format
  :ober/jira/users
  :std/format
  :std/iter
  :std/pregexp
  :std/sugar
  :std/text/json)

(export #t)

;;;; Issue operations

(def (issue-get config id)
  "Display issue details"
  (let ((body (jira-get config (format "/rest/api/3/issue/~a" id))))
    (when (hash-table? body)
      (let-hash body
        (when (hash-table? .?fields)
          (let-hash .fields
            (display-item "Key" ..key)
            (display-item "Summary" .?summary)
            (display-item "Status" (if (hash-table? .?status) (hash-get .status 'name) ""))
            (display-item "Priority" (if (hash-table? .?priority) (hash-get .priority 'name) ""))
            (display-item "Type" (if (hash-table? .?issuetype) (hash-get .issuetype 'name) ""))
            (display-item "Assignee"
              (if (hash-table? .?assignee) (hash-get .assignee 'displayName) "Unassigned"))
            (display-item "Reporter"
              (if (hash-table? .?reporter) (hash-get .reporter 'displayName) ""))
            (display-item "Project"
              (if (hash-table? .?project) (hash-get .project 'name) ""))
            (display-item "Labels"
              (if (list? .?labels) (string-join (map ->string .labels) ", ") ""))
            (display-item "Created" (date-short .?created))
            (display-item "Updated" (date-short .?updated))
            (display-item "Description" (adf-to-text .?description))
            ;; Subtasks
            (when (and .?subtasks (pair? .subtasks))
              (displayln "")
              (displayln "Subtasks:")
              (let ((headers ["Key" "Summary" "Status" "Priority"])
                    (rows []))
                (for (st .subtasks)
                  (when (hash-table? st)
                    (let-hash st
                      (when (hash-table? .?fields)
                        (let-hash .fields
                          (set! rows (cons [..?key
                                           (or .?summary "")
                                           (if (hash-table? .?status) (hash-get .status 'name) "")
                                           (if (hash-table? .?priority) (hash-get .priority 'name) "")]
                                          rows)))))))
                (output headers (reverse rows))))
            ;; Comments
            (when (hash-table? .?comment)
              (let-hash .comment
                (when (and .?comments (pair? .comments))
                  (displayln "")
                  (displayln "Comments:")
                  (for (c .comments)
                    (when (hash-table? c)
                      (let-hash c
                        (let ((author (if (hash-table? .?author)
                                        (hash-get .author 'displayName)
                                        "Unknown")))
                          (displayln (format "  [~a] ~a:" (date-short .?updated) author))
                          (displayln (format "    ~a" (adf-to-text .?body)))
                          (displayln ""))))))))))))))

(def (issue-create config project summary description)
  "Create a new issue"
  (let* ((data (hash
                ("fields"
                 (hash
                  ("project" (hash ("key" project)))
                  ("summary" summary)
                  ("issuetype" (hash ("name" "Task")))
                  ("description" (text-to-adf description))))))
         (body (jira-post config "/rest/api/3/issue" data)))
    (when (hash-table? body)
      (let-hash body
        (displayln (format "Created: ~a" .?key))
        (when .?self
          (displayln (format "URL: ~a" .self)))))))

(def (issue-edit config id fields-data)
  "Edit an issue's fields"
  (jira-put config (format "/rest/api/3/issue/~a" id)
            (hash ("fields" fields-data)))
  (displayln (format "Updated issue ~a" id)))

(def (issue-delete config id)
  "Delete an issue"
  (jira-delete config (format "/rest/api/3/issue/~a" id))
  (displayln (format "Deleted issue ~a" id)))

(def (issue-assign config id user)
  "Assign issue to user"
  (let ((account-id (name-to-id config user)))
    (jira-put config (format "/rest/api/3/issue/~a/assignee" id)
              (hash ("accountId" (or account-id user))))
    (displayln (format "Assigned ~a to ~a" id user))))

(def (issue-label config id label)
  "Add a label to an issue"
  (jira-put config (format "/rest/api/3/issue/~a" id)
            (hash ("fields" (hash ("labels" [label])))))
  (displayln (format "Added label '~a' to ~a" label id)))

(def (issue-update-field config id field content)
  "Update a custom field on an issue"
  (jira-put config (format "/rest/api/3/issue/~a" id)
            (hash ("fields" (hash (field content)))))
  (displayln (format "Updated field '~a' on ~a" field id)))

;;;; Changelog

(def (issue-changelog config id)
  "Get issue changelog"
  (let ((body (jira-get config (format "/rest/api/3/issue/~a/changelog" id)))
        (headers ["Id" "Author" "Created" "Field" "From" "To"])
        (rows []))
    (when (hash-table? body)
      (let-hash body
        (when .?values
          (for (change .values)
            (when (hash-table? change)
              (let-hash change
                (let ((author (if (hash-table? .?author)
                                (hash-get .author 'displayName)
                                "Unknown")))
                  (when .?items
                    (for (item .items)
                      (when (hash-table? item)
                        (let-hash item
                          (set! rows (cons [(or ..?id "")
                                           author
                                           (date-short ..?created)
                                           (or .?field "")
                                           (or .?fromString "")
                                           (or .?toString "")]
                                          rows)))))))))))))
    (output headers (reverse rows))))

;;;; Transitions

(def (issue-transitions config id)
  "List available transitions for an issue"
  (let ((body (jira-get config (format "/rest/api/3/issue/~a/transitions" id)))
        (headers ["Id" "Name" "To Name" "To Status"])
        (rows []))
    (when (hash-table? body)
      (let-hash body
        (when .?transitions
          (for (tr .transitions)
            (when (hash-table? tr)
              (let-hash tr
                (let ((to-name (if (hash-table? .?to) (hash-get .to 'name) ""))
                      (to-desc (if (hash-table? .?to) (hash-get .to 'description) "")))
                  (set! rows (cons [(or .?id "") (or .?name "") to-name to-desc] rows)))))))))
    (output headers (reverse rows))))

(def (issue-transition config id transition-id)
  "Transition an issue to a new state"
  (jira-post config (format "/rest/api/3/issue/~a/transitions" id)
             (hash ("transition" (hash ("id" transition-id)))))
  (displayln (format "Transitioned ~a to state ~a" id transition-id)))

(def (issue-transition-comment config id transition-id comment-text)
  "Transition an issue with a comment"
  (jira-post config (format "/rest/api/3/issue/~a/transitions" id)
             (hash ("update" (hash ("comment" [(hash ("add" (hash ("body" (text-to-adf comment-text)))))])))
                   ("transition" (hash ("id" transition-id)))))
  (displayln (format "Transitioned ~a to state ~a with comment" id transition-id)))

;;;; Comments

(def (comment-list config id)
  "List comments on an issue"
  (let ((body (jira-get config (format "/rest/api/3/issue/~a/comment" id)))
        (headers ["Id" "Author" "Updated" "Body"])
        (rows []))
    (when (hash-table? body)
      (let-hash body
        (when .?comments
          (for (c .comments)
            (when (hash-table? c)
              (let-hash c
                (set! rows (cons [(or .?id "")
                                  (if (hash-table? .?author) (hash-get .author 'displayName) "")
                                  (date-short .?updated)
                                  (truncate-string (adf-to-text .?body) 60)]
                                 rows))))))))
    (output headers (reverse rows))))

(def (comment-add config id comment-text)
  "Add a comment to an issue"
  (let ((body (jira-post config (format "/rest/api/3/issue/~a/comment" id)
                         (hash ("body" (text-to-adf comment-text))))))
    (if (hash-table? body)
      (displayln (format "Comment added to ~a (id: ~a)" id (hash-get body 'id)))
      (displayln (format "Comment added to ~a" id)))))

(def (comment-update config id comment-id comment-text)
  "Update a comment on an issue"
  (jira-put config (format "/rest/api/3/issue/~a/comment/~a" id comment-id)
            (hash ("body" (text-to-adf comment-text))))
  (displayln (format "Updated comment ~a on ~a" comment-id id)))

(def (comment-delete config id comment-id)
  "Delete a comment from an issue"
  (jira-delete config (format "/rest/api/3/issue/~a/comment/~a" id comment-id))
  (displayln (format "Deleted comment ~a from ~a" comment-id id)))

;;;; Votes

(def (vote-get config id)
  "Get vote information for an issue"
  (let ((body (jira-get config (format "/rest/api/3/issue/~a/votes" id))))
    (when (hash-table? body)
      (let-hash body
        (display-item "Votes" .?votes)
        (display-item "Has Voted" (yon .?hasVoted))
        (when (and .?voters (list? .voters))
          (displayln "Voters:")
          (for (v .voters)
            (when (hash-table? v)
              (displayln (format "  ~a" (hash-get v 'displayName))))))))))

(def (vote-add config id)
  "Add your vote to an issue"
  (jira-post config (format "/rest/api/3/issue/~a/votes" id) "")
  (displayln (format "Voted on ~a" id)))

(def (vote-remove config id)
  "Remove your vote from an issue"
  (jira-delete config (format "/rest/api/3/issue/~a/votes" id))
  (displayln (format "Removed vote from ~a" id)))

;;;; Watchers

(def (watcher-list config id)
  "List watchers on an issue"
  (let ((body (jira-get config (format "/rest/api/3/issue/~a/watchers" id)))
        (headers ["Name" "Email" "Active"])
        (rows []))
    (when (hash-table? body)
      (let-hash body
        (when .?watchers
          (for (w .watchers)
            (when (hash-table? w)
              (let-hash w
                (set! rows (cons [(or .?displayName "")
                                  (or .?emailAddress "")
                                  (yon .?active)]
                                 rows))))))))
    (output headers (reverse rows))))

(def (watcher-add config id username)
  "Add a watcher to an issue"
  (let ((account-id (name-to-id config username)))
    (jira-post config (format "/rest/api/3/issue/~a/watchers" id)
               (json-object->string (or account-id username)))
    (displayln (format "Added watcher ~a to ~a" username id))))

(def (watcher-remove config id username)
  "Remove a watcher from an issue"
  (let ((account-id (name-to-id config username)))
    (jira-delete config (format "/rest/api/3/issue/~a/watchers?accountId=~a" id (or account-id username)))
    (displayln (format "Removed watcher ~a from ~a" username id))))

;;;; Worklogs

(def (worklog-list config id)
  "List worklogs for an issue"
  (let ((body (jira-get config (format "/rest/api/3/issue/~a/worklog" id)))
        (headers ["Author" "Started" "Time Spent" "Comment"])
        (rows []))
    (when (hash-table? body)
      (let-hash body
        (when .?worklogs
          (for (wl .worklogs)
            (when (hash-table? wl)
              (let-hash wl
                (set! rows (cons [(if (hash-table? .?author) (hash-get .author 'displayName) "")
                                  (or .?started "")
                                  (or .?timeSpent "")
                                  (if .?comment (truncate-string (adf-to-text .comment) 40) "")]
                                 rows))))))))
    (output headers (reverse rows))))

(def (worklog-add config id time-spent comment-text)
  "Add a worklog entry"
  (let ((data (hash ("timeSpent" time-spent))))
    (when comment-text
      (hash-put! data "comment" (text-to-adf comment-text)))
    (jira-post config (format "/rest/api/3/issue/~a/worklog" id) data)
    (displayln (format "Added worklog: ~a to ~a" time-spent id))))

;;;; Attachments

(def (attachment-get config attachment-id)
  "Get attachment metadata"
  (let ((body (jira-get config (format "/rest/api/3/attachment/~a" attachment-id))))
    (when (hash-table? body)
      (let-hash body
        (display-item "Filename" .?filename)
        (display-item "Size" .?size)
        (display-item "MIME Type" .?mimeType)
        (display-item "Author" (if (hash-table? .?author) (hash-get .author 'displayName) ""))
        (display-item "Created" (date-short .?created))
        (display-item "Content URL" .?content)))))

(def (attachment-meta config)
  "Get attachment upload limits"
  (let ((body (jira-get config "/rest/api/3/attachment/meta")))
    (when (hash-table? body)
      (let-hash body
        (display-item "Enabled" (yon .?enabled))
        (display-item "Upload Limit" .?uploadLimit)))))

(def (attachment-delete config attachment-id)
  "Delete an attachment"
  (jira-delete config (format "/rest/api/3/attachment/~a" attachment-id))
  (displayln (format "Deleted attachment ~a" attachment-id)))

;;;; Issue metadata

(def (issue-editmeta config id)
  "Get edit metadata for an issue"
  (let ((body (jira-get config (format "/rest/api/3/issue/~a/editmeta" id))))
    (output-json body)))

(def (issue-createmeta config project)
  "Get create metadata for a project"
  (let ((body (jira-get config (format "/rest/api/3/issue/createmeta?projectKeys=~a" project))))
    (output-json body)))
