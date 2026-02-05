;;; -*- Gerbil -*-
;;; Jira CLI — Agile / Jira Software API

(import
  :ober/jira/api
  :ober/jira/format
  :std/format
  :std/iter
  :std/sugar
  :std/text/json)

(export #t)

;;;; Boards

(def (board-list config)
  "List agile boards"
  (let ((body (jira-get config "/rest/agile/1.0/board"))
        (headers ["Id" "Name" "Type" "Project"])
        (rows []))
    (when (hash-table? body)
      (let-hash body
        (when (and .?values (list? .values))
          (for (b .values)
            (when (hash-table? b)
              (let-hash b
                (set! rows (cons [(or .?id "")
                                  (or .?name "")
                                  (or .?type "")
                                  (if (hash-table? .?location)
                                    (or (hash-get .location 'projectKey) "")
                                    "")]
                                 rows))))))))
    (output headers (reverse rows))))

(def (board-get config board-id)
  "Get board details"
  (let ((body (jira-get config (format "/rest/agile/1.0/board/~a" board-id))))
    (when (hash-table? body)
      (let-hash body
        (display-item "Id" .?id)
        (display-item "Name" .?name)
        (display-item "Type" .?type)
        (when (hash-table? .?location)
          (display-item "Project" (hash-get .location 'projectName)))))))

(def (board-config config board-id)
  "Get board configuration"
  (let ((body (jira-get config (format "/rest/agile/1.0/board/~a/configuration" board-id))))
    (output-json body)))

;;;; Sprints

(def (sprint-list config board-id)
  "List sprints for a board"
  (let ((body (jira-get config (format "/rest/agile/1.0/board/~a/sprint" board-id)))
        (headers ["Id" "Name" "State" "Start Date" "End Date" "Goal"])
        (rows []))
    (when (hash-table? body)
      (let-hash body
        (when (and .?values (list? .values))
          (for (s .values)
            (when (hash-table? s)
              (let-hash s
                (set! rows (cons [(or .?id "") (or .?name "") (or .?state "")
                                  (date-short .?startDate) (date-short .?endDate)
                                  (or .?goal "")]
                                 rows))))))))
    (output headers (reverse rows))))

(def (sprint-get config sprint-id)
  "Get sprint details"
  (let ((body (jira-get config (format "/rest/agile/1.0/sprint/~a" sprint-id))))
    (when (hash-table? body)
      (let-hash body
        (display-item "Id" .?id)
        (display-item "Name" .?name)
        (display-item "State" .?state)
        (display-item "Start Date" .?startDate)
        (display-item "End Date" .?endDate)
        (display-item "Complete Date" .?completeDate)
        (display-item "Goal" .?goal)))))

(def (sprint-issues config sprint-id)
  "List issues in a sprint"
  (let ((body (jira-get config (format "/rest/agile/1.0/sprint/~a/issue" sprint-id)))
        (headers ["Key" "Summary" "Status" "Assignee" "Priority"])
        (rows []))
    (when (hash-table? body)
      (let-hash body
        (when (and .?issues (list? .issues))
          (for (iss .issues)
            (when (hash-table? iss)
              (let-hash iss
                (when (hash-table? .?fields)
                  (let-hash .fields
                    (set! rows (cons [..key
                                      (or .?summary "")
                                      (if (hash-table? .?status) (hash-get .status 'name) "")
                                      (if (hash-table? .?assignee)
                                        (email-short (hash-get .assignee 'emailAddress))
                                        "")
                                      (if (hash-table? .?priority) (hash-get .priority 'name) "")]
                                     rows))))))))))
    (output headers (reverse rows))))

(def (sprint-create config board-id name (goal ""))
  "Create a new sprint"
  (let ((data (hash ("name" name) ("originBoardId" board-id))))
    (when (and goal (> (string-length goal) 0))
      (hash-put! data "goal" goal))
    (let ((body (jira-post config "/rest/agile/1.0/sprint" data)))
      (when (hash-table? body)
        (displayln (format "Created sprint '~a' (id: ~a)" (hash-get body 'name) (hash-get body 'id)))))))

(def (sprint-update config sprint-id data)
  "Update a sprint"
  (jira-put config (format "/rest/agile/1.0/sprint/~a" sprint-id) data)
  (displayln (format "Updated sprint ~a" sprint-id)))

(def (sprint-delete config sprint-id)
  "Delete a sprint"
  (jira-delete config (format "/rest/agile/1.0/sprint/~a" sprint-id))
  (displayln (format "Deleted sprint ~a" sprint-id)))

;;;; Epics

(def (epic-list config board-id)
  "List epics for a board"
  (let ((body (jira-get config (format "/rest/agile/1.0/board/~a/epic" board-id)))
        (headers ["Id" "Key" "Name" "Summary" "Done"])
        (rows []))
    (when (hash-table? body)
      (let-hash body
        (when (and .?values (list? .values))
          (for (e .values)
            (when (hash-table? e)
              (let-hash e
                (set! rows (cons [(or .?id "") (or .?key "") (or .?name "")
                                  (or .?summary "") (yon .?done)]
                                 rows))))))))
    (output headers (reverse rows))))

(def (epic-issues config epic-id)
  "List issues in an epic"
  (let ((body (jira-get config (format "/rest/agile/1.0/epic/~a/issue" epic-id)))
        (headers ["Key" "Summary" "Status" "Assignee"])
        (rows []))
    (when (hash-table? body)
      (let-hash body
        (when (and .?issues (list? .issues))
          (for (iss .issues)
            (when (hash-table? iss)
              (let-hash iss
                (when (hash-table? .?fields)
                  (let-hash .fields
                    (set! rows (cons [..key
                                      (or .?summary "")
                                      (if (hash-table? .?status) (hash-get .status 'name) "")
                                      (if (hash-table? .?assignee)
                                        (hash-get .assignee 'displayName)
                                        "")]
                                     rows))))))))))
    (output headers (reverse rows))))

;;;; Backlog

(def (board-backlog config board-id)
  "List backlog issues for a board"
  (let ((body (jira-get config (format "/rest/agile/1.0/board/~a/backlog" board-id)))
        (headers ["Key" "Summary" "Status" "Priority"])
        (rows []))
    (when (hash-table? body)
      (let-hash body
        (when (and .?issues (list? .issues))
          (for (iss .issues)
            (when (hash-table? iss)
              (let-hash iss
                (when (hash-table? .?fields)
                  (let-hash .fields
                    (set! rows (cons [..key
                                      (or .?summary "")
                                      (if (hash-table? .?status) (hash-get .status 'name) "")
                                      (if (hash-table? .?priority) (hash-get .priority 'name) "")]
                                     rows))))))))))
    (output headers (reverse rows))))
