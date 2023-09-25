;; -*- Gerbil -*-
;;; © ober
;;; Jira client Library

(import
  :clan/text/yaml
  :gerbil/gambit
  :ober/oberlib
  :std/crypto/cipher
  :std/format
  :std/generic/dispatch
  :std/iter
  :std/misc/list
  :std/misc/ports
  :std/pregexp
  :std/srfi/13
  :std/sugar
  :std/text/base64
  :std/text/json
  )

(export #t)

(declare (not optimize-dead-definitions))
(def version "0.22")

(def config-file "~/.jira.yaml")
(import (rename-in :gerbil/gambit/os (current-time builtin-current-time)))
(import (rename-in :gerbil/gambit/os (time mytime)))
(def program-name "jira")

(def user-to-id #f)
(def id-to-user #f)

(def (load-config)
  (let ((config (hash)))
    ;;(load-user-hashes)
    (hash-for-each
     (lambda (k v)
       (hash-put! config (string->symbol k) v))
     (car (yaml-load config-file)))
    (let-hash config
      (hash-put! config 'style (or .?style "org-mode"))
      (when .?secrets
	(let-hash (u8vector->object (base64-decode .secrets))
	  (let ((password (get-password-from-config .key .iv .password)))
            (hash-put! config 'basic-auth (make-basic-auth ..?user password))))))
    config))

(def (q alias)
  (let-hash (load-config)
    (if .?queries
      (let ((cql (hash-get .queries alias)))
	(if cql
	  (search cql)
	  (begin
	    (displayln "Error: could not find alias " alias " in your ~/.jira.yaml")
	    (exit 2))))
      (displayln "Error: no queries defined in ~/.jira.yaml"))))

(def (gettoken)
  (let-hash (load-config)
    (let* ((url (format "~a/rest/api/2/serverinfo" .url))
	   (results (rest-call 'get url (default-headers .basic-auth))))
      (with ([status body] results)
        (unless status
          (error body))
        (present-item body)))))

(def (filters)
  (let-hash (load-config)
    (let* ((url (format "~a/rest/api/2/filter" .url))
	   (results (rest-call 'get url (default-headers .basic-auth))))
      (with ([status body] results)
        (unless status
          (error body))
        (when (list? body)
          (for (filter body)
            (pi filter)))))))

(def (transitions issue)
  (let-hash (load-config)
    (let* ((outs [["id" "name" "toname" "tostate"]])
	   (url (format "~a/rest/api/2/issue/~a/transitions" .url issue))
	   (results (rest-call 'get url (default-headers .basic-auth))))
      (with ([status body] results)
        (unless status
          (error body))
        (let-hash body
          (for (transition .transitions)
            (let-hash transition
              (let-hash .to
                (set! outs (cons [ ..id ..name .name .description ] outs))))))
        (style-output outs .style)))))

(def (transitions-fields issue)
  (let-hash (load-config)
    (let* ((outs [["id" "name" "toname" "tostate"]])
	   (url (format "~a/rest/api/2/issue/~a/transitions?expand=transitions.fields" .url issue))
	   (results (rest-call 'get url (default-headers .basic-auth))))
      (with ([status body] results)
        (unless status
          (error body))
        (let-hash body
          (for (transition .transitions)
            (let-hash transition
              (hash-for-each
               (lambda (k v)
                 (pi v)
                 ) .fields))))))))

(def (createmetas project basic-auth url)
  (let (url (format "~a/rest/api/2/issue/createmeta?projectKeys=~a" url project))
    (with ([status body] (rest-call 'get url (default-headers basic-auth)))
      (unless status
        (error body))
      body)))

(def (issuemetas basic-auth url)
  (let (url (format "~a/rest/api/2/issue/createmeta" url))
    (with ([status body] (rest-call 'get url (default-headers basic-auth)))
      (unless status
        (error body))
      body)))

(def (transition issue trans)
  (let-hash (load-config)
    (let ((url (format "~a/rest/api/2/issue/~a/transitions" .url issue))
          (data (hash ("transition" (hash ("id" trans))))))
      (with ([ status body ] (rest-call 'post url (default-headers .basic-auth) (json-object->string data)))
        (unless status
          (error body))
        (present-item body)))))

(def (transition-with-field issue trans field value)
  (let-hash (load-config)
    (let* ((url (format "~a/rest/api/2/issue/~a/transitions" .url issue))
	   (data (hash
                  ("update" (hash (field [ (hash ("add" (hash ("comment" value)))) ])))
		  ("transition" (hash ("id" trans))))))
      (with ([status body] (rest-call 'post url (default-headers .basic-auth) (json-object->string data)))
        (unless status
          (error body))
        (present-item body)))))

(def (transition-comment issue trans comment)
  (let-hash (load-config)
    (let* ((url (format "~a/rest/api/2/issue/~a/transitions" .url issue))
           (data (hash
                  ("update" (hash ("comment" [ (hash ("add" (hash ("body" comment)))) ])))
        	  ("transition" (hash ("id" trans))))))
      (with ([status body] (rest-call 'post url (default-headers .basic-auth) (json-object->string data)))
        (unless status
          (error body))
        (present-item body)))))

(def (watcher-del issue name)
  (let-hash (load-config)
    (make-user-to-id-hash)
    (let ((url (format "~a/rest/api/2/issue/~a/watchers?accountId=~a" .url issue (hash-get user-to-id name))))
      (with ([status body] (rest-call 'delete url (default-headers .basic-auth)))
        (present-item body)))))

(def (watcher-add issue name)
  (let-hash (load-config)
    (make-user-to-id-hash)
    (let ((url (format "~a/rest/api/2/issue/~a/watchers" .url issue))
          (add (hash-get user-to-id name)))
      (with ([status body] (rest-call 'post url (default-headers .basic-auth) (json-object->string add)))
        (unless status
          (error body))
        (present-item body)))))

(def (watchers issue)
  (let-hash (load-config)
    (let ((out [[ "Name" "Email" "Active? " ]])
          (url (format "~a/rest/api/2/issue/~a/watchers" .url issue)))
      (with ([status body] (rest-call 'get url (default-headers .basic-auth)))
        (unless status
          (error body))
        (let-hash body
          (for (watcher .watchers)
            (let-hash watcher
              (set! out (cons [ .?displayName .?emailAddress (if .active "Yes" "No") ] out))))))
      (style-output out .style))))

(def (issuetype type)
  (displayln "here: " type)
  (let-hash (load-config)
    (let ((url (format "~a/rest/api/2/issuetype/~a" .url type)))
      (with ([status body] (rest-call 'get url (default-headers .basic-auth)))
        (unless status
          (error body))
        (present-item body)))))

(def (create-issue fields)
  (make-user-to-id-hash)
  (let-hash (load-config)
    (for (k (hash-keys fields))
      (let ((val (hash-get fields k)))
        (display k)
        (if (table? val)
          (displayln (hash->list val))
          (displayln val))))
    (let ((url (format "~a/rest/api/2/issue" .url))
           (assigneeId (hash-get user-to-id (hash-get fields "assignee"))))
      (hash-put! fields "assignee" (hash ("accountId" assigneeId)))
      ;; (hash-put! fields "assignee"
      ;; (fields (hash
      ;;               ("project" (hash ("id" project)))
      ;;               ("summary" summary)
      ;;               ("issuetype" (hash ("id" issuetype)))
      ;;               ("assignee" (hash ("accountId" (hash-get user-to-id assignee))))
      ;;               ("priority" (hash ("name" priority)))
      ;;               ("customfield_17245" (hash ("id" "21451")))
      ;;               ("customfield_15014" (hash ("id" "17048")))
      ;;               ("labels" [])
      ;;               ("timetracking" (hash
      ;;                                ("originalEstimate" originalestimate)))
      ;;               ("description" description)
      ;;               ("duedate" duedate))))

;;      (when parent
;;        (hash-put! fields "parent" (hash ("id" parent))))
      ;; (hash-put! fields issuetype (hash ("id" issuetype)))

      (with ([status body] (rest-call 'post url (default-headers .basic-auth) (json-object->string (hash (fields fields)))))
        (unless status
          (error body))
        (if (table? body)
          (let-hash body
            .key))))))

(def (error-print msg (code 2))
  (displayln "Error: " msg)
  (exit code))

(def (converge-template template metas project)
  (if (not (table? template))
    (error-print "Not a table")
    (let ((converged (hash)))
      (for (k (hash-keys template))
        (hash-put! converged k (interpol-from-env (hash-get template k))))
      converged)))

     ;;  (assignee (interpol-from-env (hash-get template "assignee")))
     ;; (description (interpol-from-env (hash-get template "description")))
     ;; (duedate (interpol-from-env (hash-get template "duedate")))
     ;; (issuetype (get-issuetype-id (interpol-from-env (hash-get template "issuetype")) metas))
     ;; (labels [(interpol-from-env (hash-get template "labels"))])
     ;; (originalestimate (interpol-from-env (hash-get template "estimate")))
     ;; (priority (interpol-from-env (hash-get template "priority")))
     ;; (project (interpol-from-env (hash-get template "project")))
     ;; (summary (interpol-from-env (hash-get template "summary"))))))

(def (execute-template template metas project parent)
  (if (not (table? template))
    (begin
      (displayln "Error: execute-template passed non-table :"  template)
      (exit 2)))
  (let ((converged (converge-template template metas project)))
    (let ((parent2 (create-issue converged))
          (subtasks (hash-get template "subtasks")))
      (when subtasks
        (for (subtask subtasks)
          (execute-template subtask metas projects parent2)))
      (unless parent
        (displayln "Primary issue: " parent2)))))

(def (run creation)
  (let-hash (load-config)
    (let ((metas (createmetas .project-key .basic-auth .url)))
      (if .?creations
        (let ((creature (hash-get .creations creation)))
          (if creature
            (execute-template creature metas .project-key #f)
            (error "Error: could not find an entry for " creation " in your ~/.jira.yaml under the creations block")))
        (displayln "Error: no create templates defined in ~/.jira.yaml under creations")))))

(def (create project summary description)
  (let-hash (load-config)
    (let* ((metas (createmetas .project .basic-auth .url))
           (url (format "~a/rest/api/2/issue" .url))
           (data (hash
                  ("fields"
                   (hash
                    ("project" (hash ("id" "10071")))
                    ("summary" summary)
                    ("issuetype" (hash ("id" "3"))) ;; task == 3
                    ("assignee" (hash ("name" .user)))
                    ;;	    ("components" [ (hash ("name" component)) ])
                    ("priority" (hash ("name" "Medium-P3")))
                    ("labels" [
                               ;; (format "~a-is-working-on" .user) ;; some default tag here
                               ])
                    ("timetracking" (hash
                                     ("originalEstimate" "10")))
                    ("description" description)
                    ("duedate" "2018-05-15"))))))
      (with ([status body] (rest-call 'post url (default-headers .basic-auth) (json-object->string data)))
        (unless status
          (error body))
        (present-item body)))))

(def (get-project-id name metas)
  (let-hash metas
    (let-hash (car .projects)
      .?id)))

(def (get-issuetype-id name metas)
  (let-hash metas
    (when (pair? .?projects)
      (pp (hash->list (car .projects)))))
  (when (table? metas)
    (let-hash metas
      (let-hash (nth 0 .projects)
        (let ((id 0))
          (for (issuetype .issuetypes)
            (let-hash issuetype
              (when (string=? .name name)
                (set! id .id))))
          id)))))

(def (metas)
  (let-hash (load-config)
    (let ((metas (createmetas .project-key .basic-auth .url))
          (outs [[ "Id" "Name"  "Untranslated Name" "Description" "Subtask" "Icon Url" "Url" ]]))
      (let-hash metas
        (when (table? .?projects)
          (let-hash (car .projects)
            (for (its .issuetypes)
              (when (table? its)
                (let-hash its
                  (set! outs (cons [ .?id
                                     .?name
                                     .?untranslatedName
                                     .?description
                                     (yon .?subtask)
                                     .?iconUrl
                                     .?self ] outs))))))))
        (style-output outs .style))))

(def (fields)
  (let-hash (load-config)
    (let ((url (format "~a/rest/api/2/field" .url))
          (outs [[ "Id" "Name" "Key" "Schema" "Clause Names" "Custom?" "Long Name" "navigable?" "Searchable?" "Orderable?" ]]))
      (with ([status body] (rest-call 'get url (default-headers .basic-auth)))
        (unless status
          (error body))
        (when (list? body)
          (for (field body)
            (when (table? field)
              (let-hash field
                (set! outs (cons [ .?id
                                   .?name
                                   .?key
                                   (json-object->string .?schema)
                                   (if (list? .?clauseNames) (string-join .?clauseNames ",") .?clauseNames)
                                   (yon .?custom)
                                   .?untranslatedName
                                   (yon .?navigable)
                                   (yon .?searchable)
                                   (yon .?orderable) ] outs)))))))
      (style-output outs .style))))

(def (editmeta issue)
  (let-hash (load-config)
    (let ((url (format "~a/rest/api/2/issue/~a" .url issue)))
      (with ([status body] (rest-call 'get url (default-headers .basic-auth)))
        (unless status
          (error body))
        (present-item body)))))

(def (label issue label)
  (let-hash (load-config)
    (let ((url (format "~a/rest/api/2/issue/~a" .url issue))
          (data (hash
                 ("fields"
                  (hash
                   ("labels" [ label ]))))))
      (with ([status body] (rest-call 'put url (default-headers .basic-auth) (json-object->string data)))
        (unless status
          (error body))
        (present-item body)))))

(def (update-field issue field content)
  (let-hash (load-config)
    (let* ((data (hash))
           (fields (hash))
           (url (format "~a/rest/api/2/issue/~a" .url issue)))
      (hash-put! fields field content)
      (hash-put! data "fields" fields)
      (with ([status body] (rest-call 'put url (default-headers .basic-auth) (json-object->string data)))
        (unless status
          (error body))
        (present-item body)))))

(def (search query)
  (let-hash (load-config)
    (let* ((outs [])
           (sf .?search-fields)
           (df [ "key" "summary" "priority" "updated" "labels" "status" "assignee" "creator" "reporter" "issuetype" "project" "watchers" "url" ])
           (query
            (if (or (string-contains query "=")
                    (string-contains query "("))
              (format "~a" query)
              (format "text ~~ '~a'" query)))
           (url (format "~a/rest/api/2/search" .?url))
           (headers (if (and sf
                             (list? sf)
                             (length>n? sf 1))
                      sf
                      df)))
      (let lp ((offset 0))
        (let ((data (hash
                     ("maxResults" 100)
                     ("startAt" offset)
                     ("jql" query))))
          (with ([ status body ]
                 (rest-call 'post (format "~a?startAt=~a" url offset) (default-headers .basic-auth) (json-object->string data) 3))
            (unless status
              (error body))
            (if (table? body)
              (let-hash body
                (set! outs (cons headers outs))
                (for (iss .issues)
                  (let-hash iss
                    (dp (hash->list .fields))
                    (let-hash .fields
                      (set! outs
                        (cons
                         (filter-row-hash
                          (hash
                           ("key" ..key)
                           ("description" (when .?description (org-table-safe .description)))
                           ("summary" (when .?summary (org-table-safe .summary)))
                           ("priority" (when (table? .?priority) (hash-ref .?priority 'name)))
                           ("updated" (when .?updated (date->custom .updated)))
                           ("labels" .?labels)
                           ("status" (when (table? .?status) (hash-ref .status 'name)))
                           ("assignee" (when (table? .?assignee) (let-hash .assignee (email-short .?emailAddress))))
                           ("creator" (when (table? .?creator) (let-hash .creator (email-short .?emailAddress))))
                           ("reporter" (when (table? .?reporter) (let-hash .reporter (email-short .?emailAddress))))
                           ("issuetype" (when (table? .?issuetype) (let-hash .issuetype .?name)))
                           ("project" (when (table? .?project) (let-hash .project .?name)))
                           ("watchers" (hash-ref .watches 'watchCount))
                           ("url" (format "~a/browse/~a" ....url ..key))) headers) outs)))))
                (when (> .?total (+ offset .maxResults))
                  (lp (+ offset .maxResults))))))))
      (style-output outs "org-mode"))))

(def (email-short email)
  "Return the username left of the @"
  (if (and email
           (string-contains email "@"))
    (car (pregexp-split "@" email))
    email))

(def (comment issue comment)
  (let-hash (load-config)
    (let* ((url (format "~a/rest/api/2/issue/~a/comment" .url issue))
           (fixed-comment (convert-users-to-ids comment))
           (data (hash
                  ("body" fixed-comment))))
      (with ([status body] (rest-call 'post url (default-headers .basic-auth) (json-object->string data)))
        (unless status
          (error body))
        (present-item body)))))

(def (assign issue user)
  (let-hash (load-config)
    (let* ((url (format "~a/rest/api/2/issue/~a/assignee" .url issue))
           (id (name-to-id user))
           (data (hash ("accountId" id))))
      (with ([status body] (rest-call 'put url (default-headers .basic-auth) (json-object->string data)))
        (unless status
          (error body))
        (present-item body)))))

(def (user pattern)
  (let-hash (load-config)
    (let ((url (format "~a/rest/api/2/user/search?username=~a" .url pattern))
          (outs [[ "User" "Email" "Full Name" "Active?" "Timezone" "Profile" ]]))
      (with ([status body] (rest-call 'get url (default-headers .basic-auth)))
        (unless status
          (error body))
        (for (user body)
          (let-hash user
            (set! outs (cons [ .?name .?emailAddress .?displayName .?active .?timeZone .?self ] outs))))
        (style-output outs .style)))))

(def (issue-parse issue)
  " Given the content of an issue, parse it and display appropriately"
  (make-user-to-id-hash)
  (let-hash (load-config)
    (when (table? issue)
      (let-hash issue
        (when (and .?fields
                   (table? .fields))
          (begin
            (let-hash .fields
              (displayln "** Summary: " .summary)
              (dp (format "XXX: creator: ~a~%" (hash->list .creator)))
              (when .?status (let-hash .?status (displayln "** Description: " .?description) (displayln "** State: " .?name)))
              (when .?priority (let-hash .?priority (displayln "** Priority: " .?name)))
              (when .?issuetype (let-hash .?issuetype   (displayln "** Issue Type: " .?name)))
              (displayln "** Labels: " (if (list? .?labels) (string-join .?labels ",") .?labels))
              (displayln "** Description: " (convert-ids-to-users .?description))
              (displayln "** Summary: " (when .?summary .summary))
              (displayln "** Last Viewed: " .?lastViewed)
              (displayln "** Created: " .?created)
              (let-hash .status (displayln "** Status: " .?name))
              (when (table? .?reporter) (let-hash .reporter (displayln "** Reporter: " .?displayName " " .?emailAddress)))
              (let-hash .project (displayln "** Project: " .?name))
              (let-hash .watches (displayln "** Watch Count: " .?watchCount))
              (when (table? .?creator) (let-hash .creator (displayln "** Creator: " .?displayName " " .?emailAddress)))
              (when ...?custom-fields
                (hash-for-each
                 (lambda (k v)
                   (let ((val (hash-get ..fields (string->symbol k))))
                     (when val
                       (when (list? val)
                         (for (v val)
                           (pi v)))
                       (displayln "** " v)
                       (displayln val))))
                 ...?custom-fields))
              (displayln "** Subtasks: ")
              (when .?subtasks
                (let ((outs [[ "Id" "Summary" "Status" "Priority" ]]))
                  (for (subtask .subtasks)
                    (let-hash subtask
                      (let-hash .fields
                        (let ((pri (if (table? .?priority)
                                     (hash-get .?priority 'name)
                                     "N/A")))
                          (set! outs (cons [ ..?key .?summary (hash-ref .status 'name) pri ] outs))))))
                  (style-output outs (or .?style "org-mode"))))
              (displayln "** Comments: ")
              (let-hash .comment
                (for (comment .comments)
                  (let-hash comment
                    (let-hash .author
                      (displayln "*** Comment: " .?displayName "  on " ..?updated " said:" ))
                    (displayln (pregexp-replace* "*" (convert-ids-to-users .body) "@")))))
              (if (table? .?assignee)
                (let-hash .assignee (displayln "** Assignee: " .?displayName " " .?accountId " " .?emailAddress))
                (displayln (format "XXX: assignee: ~a type: ~a" .?assignee (type-of .?assignee)))))))))))

(def (org-table-safe str)
  (if (string? str)
    (pregexp-replace* "\\|" str "-")
    str))

(def (issue id)
  (let-hash (load-config)
    (let ((url (format "~a/rest/api/2/issue/~a" .url id)))
      (with ([status body] (rest-call 'get url (default-headers .basic-auth)))
        (unless status
          (error body))
        (issue-parse body)))))

(def (priorities)
  (let-hash (load-config)
    (let ((url (format "~a/rest/api/2/priority" .url))
          (outs [[ "Name" "Id" "Description" "Status Color" "Url" "Icon Url" ]]))
      (with ([status body] (rest-call 'get url (default-headers .basic-auth)))
        (unless status
          (error body))
        (for (priority body)
          (let-hash priority
            (set! outs (cons [ .?name .?id .?description .?statusColor .?self .?iconUrl ] outs))))
        (style-output outs)))))

(def (index-summary)
  (let-hash (load-config)
    (let ((url (format "~a/rest/api/2/index/summary" .url)))
      (with ([status body] (rest-call 'get url (default-headers .basic-auth)))
        (unless status
          (error body))
        (present-item body)))))

(def (work issue)
  (let-hash (load-config)
    (let ((url (format "~a/rest/api/3/issue/~a/worklog" .url issue)))
      (with ([status body] (rest-call 'get url (default-headers .basic-auth)))
        (unless status
          (error body))
         (when (table? body)
           (let-hash body
             (when .?worklogs
               (for (worklog .worklogs)
                 (pi worklog)))))))))

(def (members project)
  (let-hash (load-config)
    (let* ((url (format "~a/rest/api/2/group/member?groupname=~a&includeInactiveUsers=false" .url project)))
      (with ([status body] (rest-call 'get url (default-headers .basic-auth)))
        (unless status
          (error body))
        (pi body)))))

(def (users)
  (let-hash (load-config)
    (let* ((users (users-hash))
           (outs [])
           (sf .?users-fields)
           (df [ "displayName" "emailAddress" "accountId" "active" "timeZone" "accountType" "url" ])
           (headers (if (and sf
                             (list? sf)
                             (length>n? sf 1))
                      sf
                      df)))
      (when (list? users)
        (set! outs (cons headers outs))
        (for (user users)
          (unless (table? user)
            (error "user is not a table, but a " (type-of user)))
          (let-hash user
            (set! outs
              (cons
               (filter-row-hash
                (hash
                 ("displayName" .?displayName)
                 ("emailAddress" (if .?emailAddress
                                   .emailAddress
                                   "None"))
                 ("accountId" (if .?accountId
                                .accountId
                                "None"))
                 ("active" (if .?active
                             "Active"
                             "Inactive"))
                 ("timeZone" (if .?timeZone
                               .timeZone
                               "N/A"))
                 ("accountType" (if .?accountType
                                  .accountType
                                  "N/A"))
                 ("url" .?self))
                headers) outs)))))
      (style-output outs "org-mode"))))

(def (users-hash)
  "Fetch users, or read from local cache"
  (let ((user-list "~/.jira-users")
        (users []))
    (if (modified-since? user-list (* 12 24 3600))
      (set! users (read-obj-from-file user-list))
      (begin
        (let-hash (load-config)
          (let ((url (format "~a/rest/api/2/users" .url)))
            (let lp ((offset 0))
              (with ([status body] (rest-call 'get (format "~a?startAt=~a&maxResults=1000" url offset) (default-headers .basic-auth)))
                (unless status
                  (error body))
                (when (and
                        (list? body)
                        (length>n? body 0))
                  (for (user body)
                    (when (table? user)
                      (set! users (cons user users))))
                  (lp (+ offset 1000))))))
          (write-obj-to-file user-list users))))
    users))

(def (dump-users-yaml users)
  " Write out the users hash to ~/.jira-users.yaml "
  (let ((user-list "~/.jira-users.yaml"))
    (unless (list? users)
      (error "Users is not list!"))
    (try
     (yaml-dump user-list users)
     (catch (e)
       (raise e)))))

(def (configuration)
  "Return the configuration of the Jira server"
  (let-hash (load-config)
    (let ((url (format "~a/rest/api/2/configuration" .url))
          (outs [[ "Id" "Key" "Name" "Type" ]]))
      (with ([status body] (rest-call 'get url (default-headers .basic-auth)))
        (unless status
          (error body))
        (when (table? body)
          (let-hash body
            (pi .?timeTrackingConfiguration)))))))

(def (projects)
  (let-hash (load-config)
    (let ((url (format "~a/rest/api/2/project" .url))
          (outs [[ "Id" "Key" "Name" "Type" ]]))
      (with ([status body] (rest-call 'get url (default-headers .basic-auth)))
        (unless status
          (error body))
        (for (project body)
          (when (table? project)
            (let-hash project
              (set! outs (cons [ .?id .?key .?name .?projectTypeKey ] outs))))))
      (style-output outs))))

(def (properties issue)
  (let-hash (load-config)
    (let ((url (format "~a/rest/api/3/issue/~a/properties" .url (string-upcase issue))))
      (with ([status body] (rest-call 'get url (default-headers .basic-auth)))
        (unless status
          (error body))
        (when (table? body)
          (let-hash body
            (when .?keys
              (when (list? .keys)
                (for (property .keys)
                  (when (table? property)
                    (let-hash property
                      (displayln .?key " " .?self))))))))))))

(def (get-new-ip uri host)
  (pregexp-replace "[0-9]{1,3}\\.[0-9]{1,3}\\.[0-9]{1,3}\\.[0-9]{1,3}" uri (resolve-ipv4 host)))

(def (config)
  (let-hash (load-config)
    (display "What is your Jira password? (will not echo) :")
    (let* ((password (read-password ##console-port))
           (cipher (make-aes-256-ctr-cipher))
           (iv (random-bytes (cipher-iv-length cipher)))
           (key (random-bytes (cipher-key-length cipher)))
           (encrypted-password (encrypt cipher key iv password))
           (enc-pass-store (u8vector->base64-string encrypted-password))
           (iv-store (u8vector->base64-string iv))
           (key-store (u8vector->base64-string key))
           (secrets (base64-encode (object->u8vector
                                    (hash
                                     (password enc-pass-store)
                                     (iv iv-store)
                                     (key key-store))))))

      (displayln "Add the following lines to your " config-file)
      (displayln "")
      (displayln "secrets: " secrets))))

(def (get-password-from-config key iv password)
  (bytes->string
   (decrypt
    (make-aes-256-ctr-cipher)
    (base64-string->u8vector key)
    (base64-string->u8vector iv)
    (base64-string->u8vector password))))

(def (red txt)
  "Return a red version of txt"
  (format "\\e[7;37;41m~a\\e[o;37;40m" txt))

(def (open issue)
  (let-hash (load-config)
    (let* ((command (cond-expand
                      (darwin "open")
                      (linux "xdg-open")
                      (bsd "xdg-open")))
           (job (format "~a ~a/browse/~a" command .url issue)))
      (displayln (shell-command job)))))

(def (name-to-id name)
  "Return the accountid associated with username"
  (let ((users (users-hash))
        (id #f))
    (for (user users)
      (unless id
        (let-hash user
          (when .?emailAddress
            (let ((short (car (pregexp-split "@" .?emailAddress))))
              (when (and short
                         (string=? short name))
                (set! id .accountId)))))))
    id))

(def (make-user-to-id-hash)
  "Make users hash and set global user-to-id"
  (unless user-to-id
    (set! user-to-id (hash))
    (set! id-to-user (hash)))
  (let ((users (users-hash)))
    (for (user users)
      (let-hash user
        (let ((short (email-short .?emailAddress)))
          (hash-put! user-to-id short .?accountId)
          (hash-put! id-to-user .?accountId short))))))

(def (convert-ids-to-users str)
  (when (string? str)
    (unless (and
              id-to-user
              user-to-id)
      (make-user-to-id-hash))
    (let ((re "(?:^|\\s)(?:\\[\\~accountid:)([0-9A-Za-z-:]+)(?:\\])")
          (delim "~accountid:")
          (fmt " @~a")
          (fstr (pregexp-replace* "\\]\\[" str "] [")))
      (hash-interpol re delim fstr id-to-user fmt))))

(def (convert-users-to-ids str)
  (unless (and
            id-to-user
            user-to-id)
    (make-user-to-id-hash))
  (let ((re "(?:^|\\s)@([a-zA-Z0-9]*)")
        (delim "@")
        (fmt " [~~accountid:~a]"))
    (hash-interpol re delim str user-to-id fmt)))

(def (convert-names str)
  (let ((results ""))
    (pregexp-replace* "(\\s|^)@([a-zA-Z0-9]+)" str "\\1\\[\\~\\2\\]")))

(def (default-headers basic)
  [
   ["Accept" :: "*/*"]
   ["Content-type" :: "application/json"]
   ["Authorization" :: basic ]
   ])
