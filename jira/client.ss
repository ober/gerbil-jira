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
  :std/net/uri
  :std/pregexp
  :std/srfi/13
  :std/sugar
  :std/text/base64
  :std/text/json
  )

(export #t)

(declare (not optimize-dead-definitions))
(def version "0.23")

(def config-file "~/.jira.yaml")
(def program-name "jira")
(def keys-dir (path-expand "~/.config/gerbil/keys"))
(def key-file (path-expand "jira.key" keys-dir))

(def user-to-id #f)
(def id-to-user #f)

;; Security: Ensure directory exists with restricted permissions
(def (ensure-keys-dir!)
  (unless (file-exists? keys-dir)
    (create-directory* keys-dir)
    (##shell-command (format "chmod 700 ~a" keys-dir))))

;; Security: Save key to separate file with restricted permissions
(def (save-key-to-file! key-bytes)
  (ensure-keys-dir!)
  (let ((key-b64 (u8vector->base64-string key-bytes)))
    (with-output-to-file [path: key-file create: 'maybe truncate: #t]
      (lambda () (display key-b64)))
    (##shell-command (format "chmod 400 ~a" key-file))
    (displayln (format "Key saved to ~a (mode 0400)" key-file))))

;; Security: Load key from separate file
(def (load-key-from-file)
  (unless (file-exists? key-file)
    (error (format "Key file not found: ~a. Run 'config' command first." key-file)))
  (base64-string->u8vector (read-file-string key-file)))

;; Security: Check file permissions (warn if too permissive)
(def (check-file-permissions! file)
  (when (file-exists? file)
    (let* ((info (file-info file))
           (mode (file-info-mode info)))
      (when (> (bitwise-and mode #o077) 0)
        (displayln (format "WARNING: ~a has insecure permissions. Run: chmod 600 ~a" file file))))))

(def (load-config)
  ;; SECURITY: Check file permissions
  (check-file-permissions! config-file)
  (check-file-permissions! key-file)

  (let ((config (hash)))
    ;;(load-user-hashes)
    (hash-for-each
     (lambda (k v)
       (hash-put! config (string->symbol k) v))
     (car (yaml-load config-file)))
    (let-hash config
      (hash-put! config 'style (or .?style "org-mode"))
      (when .?secrets
        ;; Use JSON parsing instead of u8vector->object for security
        ;; Prevents arbitrary code execution from malicious config files
        (let ((secrets-json (parameterize ((read-json-key-as-symbol? #t))
                              (with-input-from-string
                                  (bytes->string (base64-decode .secrets))
                                read-json))))
          (let-hash secrets-json
            ;; SECURITY FIX: Check if key is in secrets (legacy) or separate file (secure)
            (let ((password (if .?key
                              ;; Legacy format: key in config (insecure, warn user)
                              (get-password-from-config-legacy .key .iv .password)
                              ;; New format: key in separate file (secure)
                              (get-password-from-config .iv .password))))
              (hash-put! config 'basic-auth (make-basic-auth ..?user password)))))))
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
    (let* ((url (format "~a/rest/api/3/serverinfo" .url))
	       (results (rest-call 'get url (default-headers .basic-auth))))
      (with ([status body] results)
        (unless status
          (error body))
        (present-item body)))))

(def (filters)
  (let-hash (load-config)
    (let* ((url (format "~a/rest/api/3/filter" .url))
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
	       (url (format "~a/rest/api/3/issue/~a/transitions" .url issue))
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
	       (url (format "~a/rest/api/3/issue/~a/transitions?expand=transitions.fields" .url issue))
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
  (let (url (format "~a/rest/api/3/issue/createmeta?projectKeys=~a" url project))
    (with ([status body] (rest-call 'get url (default-headers basic-auth)))
      (unless status
        (error body))
      body)))

(def (issuemetas basic-auth url)
  (let (url (format "~a/rest/api/3/issue/createmeta" url))
    (with ([status body] (rest-call 'get url (default-headers basic-auth)))
      (unless status
        (error body))
      body)))

(def (transition issue trans)
  (let-hash (load-config)
    (let ((url (format "~a/rest/api/3/issue/~a/transitions" .url issue))
          (data (hash ("transition" (hash ("id" trans))))))
      (with ([ status body ] (rest-call 'post url (default-headers .basic-auth) (json-object->string data)))
        (unless status
          (error body))
        (present-item body)))))

(def (transition-with-field issue trans field value)
  (let-hash (load-config)
    (let* ((url (format "~a/rest/api/3/issue/~a/transitions" .url issue))
	       (data (hash
                  ("update" (hash (field [ (hash ("add" (hash ("comment" value)))) ])))
		          ("transition" (hash ("id" trans))))))
      (with ([status body] (rest-call 'post url (default-headers .basic-auth) (json-object->string data)))
        (unless status
          (error body))
        (present-item body)))))

(def (transition-comment issue trans comment)
  (let-hash (load-config)
    (let* ((url (format "~a/rest/api/3/issue/~a/transitions" .url issue))
           (data (hash
                  ("update" (hash ("comment" [ (hash ("add" (hash ("body" (text-to-adf comment))))) ])))
        	      ("transition" (hash ("id" trans))))))
      (with ([status body] (rest-call 'post url (default-headers .basic-auth) (json-object->string data)))
        (unless status
          (error body))
        (present-item body)))))

(def (watcher-del issue name)
  (let-hash (load-config)
    (make-user-to-id-hash)
    (let ((url (format "~a/rest/api/3/issue/~a/watchers?accountId=~a" .url issue (hash-get user-to-id name))))
      (with ([status body] (rest-call 'delete url (default-headers .basic-auth)))
        (unless status
          (error body))
        (present-item body)))))

(def (watcher-add issue name)
  (let-hash (load-config)
    (make-user-to-id-hash)
    (let ((url (format "~a/rest/api/3/issue/~a/watchers" .url issue))
          (add (hash-get user-to-id name)))
      (with ([status body] (rest-call 'post url (default-headers .basic-auth) (json-object->string add)))
        (unless status
          (error body))
        (present-item body)))))

(def (watchers issue)
  (let-hash (load-config)
    (let ((out [[ "Name" "Email" "Active? " ]])
          (url (format "~a/rest/api/3/issue/~a/watchers" .url issue)))
      (with ([status body] (rest-call 'get url (default-headers .basic-auth)))
        (unless status
          (error body))
        (let-hash body
          (for (watcher .watchers)
            (let-hash watcher
              (set! out (cons [ .?displayName .?emailAddress (if .active "Yes" "No") ] out))))))
      (style-output out .style))))

(def (issuetype type)
  (let-hash (load-config)
    (let ((url (format "~a/rest/api/3/issuetype/~a" .url type)))
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
        (if (hash-table? val)
          (displayln (hash->list val))
          (displayln val))))
    (let ((url (format "~a/rest/api/3/issue" .url))
          (assigneeId (hash-get user-to-id (hash-get fields "assignee"))))
      (hash-put! fields "assignee" (hash ("accountId" assigneeId)))
      (with ([status body] (rest-call 'post url (default-headers .basic-auth) (json-object->string (hash (fields fields)))))
        (unless status
          (error body))
        (if (hash-table? body)
          (let-hash body
            .key))))))

(def (error-print msg (code 2))
  (displayln "Error: " msg)
  (exit code))

(def (converge-template template metas project)
  (if (not (hash-table? template))
    (error-print "Not a table")
    (let ((converged (hash)))
      (for (k (hash-keys template))
        (hash-put! converged k (interpol-from-env (hash-get template k))))
      converged)))

(def (execute-template template metas project parent)
  (if (not (hash-table? template))
    (begin
      (displayln "Error: execute-template passed non-table :"  template)
      (exit 2)))
  (let ((converged (converge-template template metas project)))
    (let ((parent2 (create-issue converged))
          (subtasks (hash-get template "subtasks")))
      (when subtasks
        (for (subtask subtasks)
          (execute-template subtask metas project parent2)))
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
    (make-user-to-id-hash)
    (let* ((metas (createmetas .project .basic-auth .url))
           (url (format "~a/rest/api/3/issue" .url))
           (assignee-id (hash-get user-to-id .user))
           (data (hash
                  ("fields"
                   (hash
                    ("project" (hash ("key" project)))
                    ("summary" summary)
                    ("issuetype" (hash ("id" "3"))) ;; task == 3
                    ("assignee" (hash ("accountId" assignee-id)))
                    ;;	    ("components" [ (hash ("name" component)) ])
                    ("priority" (hash ("name" "Medium-P3")))
                    ("labels" [
                               ;; (format "~a-is-working-on" .user) ;; some default tag here
                               ])
                    ("timetracking" (hash
                                     ("originalEstimate" "10")))
                    ("description" (text-to-adf description)))))))
      (with ([status body] (rest-call 'post url (default-headers .basic-auth) (json-object->string data)))
        (unless status
          (error body))
        (present-item body)))))

(def (get-project-id-by-key project-key basic-auth url)
  "Fetch project ID from project key for API v3 compatibility"
  (let ((api-url (format "~a/rest/api/3/project/~a" url project-key)))
    (with ([status body] (rest-call 'get api-url (default-headers basic-auth)))
      (unless status
        (error body))
      (when (hash-table? body)
        (hash-ref body 'id)))))

(def (get-project-id name metas)
  (let-hash metas
    (let-hash (car .projects)
      .?id)))

(def (get-issuetype-id name metas)
  (let-hash metas
    (when (pair? .?projects)
      (pp (hash->list (car .projects)))))
  (when (hash-table? metas)
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
        (when (hash-table? .?projects)
          (let-hash (car .projects)
            (for (its .issuetypes)
              (when (hash-table? its)
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
    (let ((url (format "~a/rest/api/3/field" .url))
          (outs [[ "Id" "Name" "Key" "Schema" "Clause Names" "Custom?" "Long Name" "navigable?" "Searchable?" "Orderable?" ]]))
      (with ([status body] (rest-call 'get url (default-headers .basic-auth)))
        (unless status
          (error body))
        (when (list? body)
          (for (field body)
            (when (hash-table? field)
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
    (let ((url (format "~a/rest/api/3/issue/~a" .url issue)))
      (with ([status body] (rest-call 'get url (default-headers .basic-auth)))
        (unless status
          (error body))
        (present-item body)))))

(def (label issue label)
  (let-hash (load-config)
    (let ((url (format "~a/rest/api/3/issue/~a" .url issue))
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
           (url (format "~a/rest/api/3/issue/~a" .url issue)))
      (hash-put! fields field content)
      (hash-put! data "fields" fields)
      (with ([status body] (rest-call 'put url (default-headers .basic-auth) (json-object->string data)))
        (unless status
          (error body))
        (present-item body)))))

(def (delete-issue issue)
  (let-hash (load-config)
    (let ((url (format "~a/rest/api/3/issue/~a" .url issue)))
      (with ([status body] (rest-call 'delete url (default-headers .basic-auth)))
        (unless status
          (error body))
        (present-item body)))))

(def (get-attachment attachment-id)
  (let-hash (load-config)
    (let ((url (format "~a/rest/api/3/attachment/~a" .url attachment-id)))
      (with ([status body] (rest-call 'get url (default-headers .basic-auth)))
        (unless status
          (error body))
        (present-item body)))))

(def (changelog issue)
  (let-hash (load-config)
    (let ((url (format "~a/rest/api/3/issue/~a/changelog" .url issue))
          (outs [[ "Id" "Author" "Created" "Field" "From" "To" ]]))
      (with ([status body] (rest-call 'get url (default-headers .basic-auth)))
        (unless status
          (error body))
        (when (hash-table? body)
          (let-hash body
            (when .?values
              (for (change .values)
                (let-hash change
                  (let ((author-name (if (hash-table? .?author)
                                       (hash-ref .author 'displayName)
                                       "Unknown")))
                    (when .?items
                      (for (item .items)
                        (let-hash item
                          (set! outs (cons [ ..?id author-name ..?created .?field .?fromString .?toString ] outs)))))))))))
        (style-output outs .style)))))

(def (delete-comment issue comment-id)
  (let-hash (load-config)
    (let ((url (format "~a/rest/api/3/issue/~a/comment/~a" .url issue comment-id)))
      (with ([status body] (rest-call 'delete url (default-headers .basic-auth)))
        (unless status
          (error body))
        (present-item body)))))

(def (update-comment issue comment-id comment-text)
  (let-hash (load-config)
    (let ((url (format "~a/rest/api/3/issue/~a/comment/~a" .url issue comment-id))
          (data (hash ("body" (text-to-adf comment-text)))))
      (with ([status body] (rest-call 'put url (default-headers .basic-auth) (json-object->string data)))
        (unless status
          (error body))
        (present-item body)))))

(def (create-link link-type inward-issue outward-issue)
  (let-hash (load-config)
    (let ((url (format "~a/rest/api/3/issueLink" .url))
          (data (hash
                 ("type" (hash ("name" link-type)))
                 ("inwardIssue" (hash ("key" inward-issue)))
                 ("outwardIssue" (hash ("key" outward-issue))))))
      (with ([status body] (rest-call 'post url (default-headers .basic-auth) (json-object->string data)))
        (unless status
          (error body))
        (present-item body)))))

(def (delete-link link-id)
  (let-hash (load-config)
    (let ((url (format "~a/rest/api/3/issueLink/~a" .url link-id)))
      (with ([status body] (rest-call 'delete url (default-headers .basic-auth)))
        (unless status
          (error body))
        (present-item body)))))

(def (get-link link-id)
  (let-hash (load-config)
    (let ((url (format "~a/rest/api/3/issueLink/~a" .url link-id)))
      (with ([status body] (rest-call 'get url (default-headers .basic-auth)))
        (unless status
          (error body))
        (present-item body)))))

(def (link-types)
  (let-hash (load-config)
    (let ((url (format "~a/rest/api/3/issueLinkType" .url))
          (outs [[ "Id" "Name" "Inward" "Outward" ]]))
      (with ([status body] (rest-call 'get url (default-headers .basic-auth)))
        (unless status
          (error body))
        (when (hash-table? body)
          (let-hash body
            (when .?issueLinkTypes
              (for (linktype .issueLinkTypes)
                (let-hash linktype
                  (set! outs (cons [ .?id .?name .?inward .?outward ] outs)))))))
        (style-output outs .style)))))

(def (myself)
  (let-hash (load-config)
    (let ((url (format "~a/rest/api/3/myself" .url)))
      (with ([status body] (rest-call 'get url (default-headers .basic-auth)))
        (unless status
          (error body))
        (present-item body)))))

(def (get-project project-key)
  (let-hash (load-config)
    (let ((url (format "~a/rest/api/3/project/~a" .url project-key)))
      (with ([status body] (rest-call 'get url (default-headers .basic-auth)))
        (unless status
          (error body))
        (present-item body)))))

(def (project-roles project-key)
  (let-hash (load-config)
    (let ((url (format "~a/rest/api/3/project/~a/role" .url project-key)))
      (with ([status body] (rest-call 'get url (default-headers .basic-auth)))
        (unless status
          (error body))
        (present-item body)))))

(def (project-versions project-key)
  (let-hash (load-config)
    (let ((url (format "~a/rest/api/3/project/~a/versions" .url project-key))
          (outs [[ "Id" "Name" "Description" "Released" "Archived" "Release Date" ]]))
      (with ([status body] (rest-call 'get url (default-headers .basic-auth)))
        (unless status
          (error body))
        (when (list? body)
          (for (version body)
            (let-hash version
              (set! outs (cons [ .?id .?name .?description (yon .?released) (yon .?archived) .?releaseDate ] outs)))))
        (style-output outs .style)))))

(def (project-components project-key)
  (let-hash (load-config)
    (let ((url (format "~a/rest/api/3/project/~a/components" .url project-key))
          (outs [[ "Id" "Name" "Description" "Lead" "Assignee Type" ]]))
      (with ([status body] (rest-call 'get url (default-headers .basic-auth)))
        (unless status
          (error body))
        (when (list? body)
          (for (component body)
            (let-hash component
              (let ((lead-name (if (hash-table? .?lead)
                                 (hash-ref .lead 'displayName)
                                 "None")))
                (set! outs (cons [ .?id .?name .?description lead-name .?assigneeType ] outs))))))
        (style-output outs .style)))))

(def (create-version project-key name description release-date)
  (let-hash (load-config)
    (let ((url (format "~a/rest/api/3/version" .url))
          (data (hash
                 ("name" name)
                 ("description" description)
                 ("projectId" (get-project-id-by-key project-key .basic-auth .url))
                 ("releaseDate" release-date))))
      (with ([status body] (rest-call 'post url (default-headers .basic-auth) (json-object->string data)))
        (unless status
          (error body))
        (present-item body)))))

(def (create-component project-key name description)
  (let-hash (load-config)
    (make-user-to-id-hash)
    (let ((url (format "~a/rest/api/3/component" .url))
          (data (hash
                 ("name" name)
                 ("description" description)
                 ("project" project-key)
                 ("leadAccountId" (hash-get user-to-id .user)))))
      (with ([status body] (rest-call 'post url (default-headers .basic-auth) (json-object->string data)))
        (unless status
          (error body))
        (present-item body)))))

(def (remote-links issue)
  (let-hash (load-config)
    (let ((url (format "~a/rest/api/3/issue/~a/remotelink" .url issue))
          (outs [[ "Id" "Title" "URL" "Relationship" ]]))
      (with ([status body] (rest-call 'get url (default-headers .basic-auth)))
        (unless status
          (error body))
        (when (list? body)
          (for (link body)
            (let-hash link
              (let ((link-url (if (hash-table? .?object)
                                (hash-ref .object 'url)
                                "N/A"))
                    (link-title (if (hash-table? .?object)
                                  (hash-ref .object 'title)
                                  "N/A")))
                (set! outs (cons [ .?id link-title link-url .?relationship ] outs))))))
        (style-output outs .style)))))

(def (create-remote-link issue url title)
  (let-hash (load-config)
    (let ((api-url (format "~a/rest/api/3/issue/~a/remotelink" .url issue))
          (data (hash
                 ("object" (hash
                            ("url" url)
                            ("title" title))))))
      (with ([status body] (rest-call 'post api-url (default-headers .basic-auth) (json-object->string data)))
        (unless status
          (error body))
        (present-item body)))))

(def (delete-remote-link issue link-id)
  (let-hash (load-config)
    (let ((url (format "~a/rest/api/3/issue/~a/remotelink/~a" .url issue link-id)))
      (with ([status body] (rest-call 'delete url (default-headers .basic-auth)))
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
           (url (format "~a/rest/api/3/search/jql" .?url))
           (headers (if (and sf
                             (list? sf)
                             (length>n? sf 1))
                      sf
                      df)))
      (set! outs (cons headers outs))
      (let lp ((offset 0))
        ;; New /search/jql endpoint uses query parameters, not POST body
        (let ((search-url (format "~a?jql=~a&maxResults=100&startAt=~a"
                                  url
                                  (uri-encode query)
                                  offset)))
          (with ([ status body ]
                 (rest-call 'get search-url (default-headers .basic-auth)))
            (unless status
              (error body))
            (when (hash-table? body)
              (let-hash body
                (when (list? .?issues)
                  (for (iss .issues)
                    (let-hash iss
                      (when (hash-table? .?fields)
                        (let-hash .fields
                          (set! outs
                            (cons
                             (filter-row-hash
                              (hash
                               ("key" ..key)
                               ("description" (if .?description (org-table-safe (adf-to-text .description)) ""))
                               ("summary" (if .?summary (org-table-safe .summary) ""))
                               ("priority" (if (hash-table? .?priority) (hash-ref .priority 'name) ""))
                               ("updated" (if .?updated (date->custom .updated) ""))
                               ("labels" (if (list? .?labels) (string-join (map (lambda (x) (format "~a" x)) .?labels) ",") ""))
                               ("status" (if (hash-table? .?status) (hash-ref .status 'name) ""))
                               ("assignee" (if (hash-table? .?assignee) (let-hash .assignee (email-short .?emailAddress)) ""))
                               ("creator" (if (hash-table? .?creator) (let-hash .creator (email-short .?emailAddress)) ""))
                               ("reporter" (if (hash-table? .?reporter) (let-hash .reporter (email-short .?emailAddress)) ""))
                               ("issuetype" (if (hash-table? .?issuetype) (let-hash .issuetype .?name) ""))
                               ("project" (if (hash-table? .?project) (let-hash .project .?name) ""))
                               ("watchers" (if (hash-table? .?watches) (hash-ref .watches 'watchCount) ""))
                               ("url" (format "~a/browse/~a" ....url ..key))) headers) outs)))))))
                ;; Paginate: fetch next page if more results exist
                (let ((returned-count (length .issues)))
                  (when (and .?total (> .total (+ offset returned-count)) (> returned-count 0))
                    (lp (+ offset returned-count)))))))))
      (style-output outs .style))))

(def (email-short email)
  "Return the username left of the @"
  (if (and email
           (string-contains email "@"))
    (car (pregexp-split "@" email))
    email))

(def (text-to-adf text)
  "Convert plain text to Atlassian Document Format (ADF) required by API v3"
  (hash
   ("type" "doc")
   ("version" 1)
   ("content" [(hash
                ("type" "paragraph")
                ("content" [(hash ("type" "text") ("text" text))]))])))

(def (adf-to-text adf)
  "Convert Atlassian Document Format (ADF) to plain text for display"
  (cond
   ((string? adf) adf)
   ((not (hash-table? adf)) (if adf (format "~a" adf) ""))
   (else
    (let ((type (hash-ref adf 'type #f))
          (content (hash-ref adf 'content #f))
          (text (hash-ref adf 'text #f)))
      (cond
       (text text)
       ((and content (list? content))
        (string-join
         (map adf-to-text content)
         (if (equal? type "paragraph") "\n" "")))
       (else ""))))))

(def (comment issue comment)
  (let-hash (load-config)
    (let* ((url (format "~a/rest/api/3/issue/~a/comment" .url issue))
           (fixed-comment (convert-users-to-ids comment))
           (data (hash
                  ("body" (text-to-adf fixed-comment)))))
      (with ([status body] (rest-call 'post url (default-headers .basic-auth) (json-object->string data)))
        (unless status
          (error body))
        (present-item body)))))

(def (assign issue user)
  (let-hash (load-config)
    (let* ((url (format "~a/rest/api/3/issue/~a/assignee" .url issue))
           (id (name-to-id user))
           (data (hash ("accountId" id))))
      (with ([status body] (rest-call 'put url (default-headers .basic-auth) (json-object->string data)))
        (unless status
          (error body))
        (present-item body)))))

(def (user pattern)
  (let-hash (load-config)
    (let ((url (format "~a/rest/api/3/user/search?query=~a" .url pattern))
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
    (when (hash-table? issue)
      (let-hash issue
        (when (and .?fields
                   (hash-table? .fields))
          (let-hash .fields
              (displayln "** Summary: " .summary)
              (when .?status (let-hash .?status (displayln "** State: " .?name)))
              (when .?priority (let-hash .?priority (displayln "** Priority: " .?name)))
              (when .?issuetype (let-hash .?issuetype   (displayln "** Issue Type: " .?name)))
              (displayln "** Labels: " (if (list? .?labels) (string-join .?labels ",") .?labels))
              (displayln "** Description: " (convert-ids-to-users (adf-to-text .?description)))
              (displayln "** Last Viewed: " .?lastViewed)
              (displayln "** Created: " .?created)
              (let-hash .status (displayln "** Status: " .?name))
              (when (hash-table? .?reporter) (let-hash .reporter (displayln "** Reporter: " .?displayName " " .?emailAddress)))
              (let-hash .project (displayln "** Project: " .?name))
              (let-hash .watches (displayln "** Watch Count: " .?watchCount))
              (when (hash-table? .?creator) (let-hash .creator (displayln "** Creator: " .?displayName " " .?emailAddress)))
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
                        (let ((pri (if (hash-table? .?priority)
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
                    (displayln (pregexp-replace* "*" (convert-ids-to-users (adf-to-text .body)) "@")))))
              (if (hash-table? .?assignee)
                (let-hash .assignee (displayln "** Assignee: " .?displayName " " .?accountId " " .?emailAddress))
                (displayln "** Assignee: unassigned"))))))))

(def (org-table-safe str)
  (if (string? str)
    (pregexp-replace* "\\|" str "-")
    str))

(def (issue id)
  (let-hash (load-config)
    (let ((url (format "~a/rest/api/3/issue/~a" .url id)))
      (with ([status body] (rest-call 'get url (default-headers .basic-auth)))
        (unless status
          (error body))
        (issue-parse body)))))

(def (priorities)
  (let-hash (load-config)
    (let ((url (format "~a/rest/api/3/priority" .url))
          (outs [[ "Name" "Id" "Description" "Status Color" "Url" "Icon Url" ]]))
      (with ([status body] (rest-call 'get url (default-headers .basic-auth)))
        (unless status
          (error body))
        (for (priority body)
          (let-hash priority
            (set! outs (cons [ .?name .?id .?description .?statusColor .?self .?iconUrl ] outs))))
        (style-output outs .style)))))

(def (index-summary)
  (let-hash (load-config)
    (let ((url (format "~a/rest/api/3/index/summary" .url)))
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
        (when (hash-table? body)
          (let-hash body
            (when .?worklogs
              (for (worklog .worklogs)
                (pi worklog)))))))))

(def (members project)
  (let-hash (load-config)
    (let* ((url (format "~a/rest/api/3/group/member?groupname=~a&includeInactiveUsers=false" .url project)))
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
          (unless (hash-table? user)
            (error "user is not a table, but a " (##type-id user)))
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
    ;; Try reading from cache if fresh
    (when (modified-since? user-list (* 12 24 3600))
      (let ((cached (read-json-from-file user-list)))
        (when (list? cached)
          (set! users cached))))
    ;; If no cached data, fetch from API
    (when (null? users)
      (let-hash (load-config)
        (let ((url (format "~a/rest/api/3/users/search" .url)))
          (let lp ((offset 0))
            (with ([status body] (rest-call 'get (format "~a?startAt=~a&maxResults=1000" url offset) (default-headers .basic-auth)))
              (unless status
                (error body))
              (when (and
                      (list? body)
                      (length>n? body 0))
                (for (user body)
                  (when (hash-table? user)
                    (set! users (cons user users))))
                (lp (+ offset 1000)))))))
      (write-json-to-file user-list users))
    users))

(def (dump-users-yaml users)
  " Write out the users hash to ~/.jira-users.yaml "
  (let ((user-list "~/.jira-users.yaml"))
    (unless (list? users)
      (error "Users is not list!"))
    (yaml-dump user-list users)))

(def (configuration)
  "Return the configuration of the Jira server"
  (let-hash (load-config)
    (let ((url (format "~a/rest/api/3/configuration" .url))
          (outs [[ "Id" "Key" "Name" "Type" ]]))
      (with ([status body] (rest-call 'get url (default-headers .basic-auth)))
        (unless status
          (error body))
        (when (hash-table? body)
          (let-hash body
            (pi .?timeTrackingConfiguration)))))))

(def (projects)
  (let-hash (load-config)
    (let ((url (format "~a/rest/api/3/project/search?startAt=0&maxResults=100" .url))
          (outs [[ "Id" "Key" "Name" "Type" ]]))
      (with ([status body] (rest-call 'get url (default-headers .basic-auth)))
        (unless status
          (error body))
        (when (hash-table? body)
          (let-hash body
            (when (list? .?values)
              (for (project .values)
                (when (hash-table? project)
                  (let-hash project
                    (set! outs (cons [ .?id .?key .?name .?projectTypeKey ] outs)))))))))
      (style-output outs .style))))

(def (properties issue)
  (let-hash (load-config)
    (let ((url (format "~a/rest/api/3/issue/~a/properties" .url (string-upcase issue))))
      (with ([status body] (rest-call 'get url (default-headers .basic-auth)))
        (unless status
          (error body))
        (when (hash-table? body)
          (let-hash body
            (when .?keys
              (when (list? .keys)
                (for (property .keys)
                  (when (hash-table? property)
                    (let-hash property
                      (displayln .?key " " .?self))))))))))))

(def (config)
  (let-hash (load-config)
    (display "What is your Jira api key? (will not echo) :")
    (let* ((password (read-password ##console-port))
           (cipher (make-aes-256-ctr-cipher))
           (iv (random-bytes (cipher-iv-length cipher)))
           (key (random-bytes (cipher-key-length cipher)))
           (encrypted-password (encrypt cipher key iv password))
           (enc-pass-store (u8vector->base64-string encrypted-password))
           (iv-store (u8vector->base64-string iv))
           ;; SECURITY FIX: Store key in separate file, NOT in config
           ;; Only store ciphertext and IV in the config file
           (secrets-hash (hash
                          ("password" enc-pass-store)
                          ("iv" iv-store)))
           (secrets (base64-encode (string->bytes (json-object->string secrets-hash)))))
      ;; Save the key to a separate file with restricted permissions
      (save-key-to-file! key)
      (displayln "")
      (displayln "Add the following lines to your " config-file)
      (displayln "")
      (displayln "secrets: " secrets)
      (displayln "")
      (displayln "SECURITY NOTE: Your encryption key is stored separately in " key-file))))

;; SECURITY FIX: Load key from separate file instead of config
(def (get-password-from-config iv password)
  (let ((key (load-key-from-file)))
    (bytes->string
     (decrypt
      (make-aes-256-ctr-cipher)
      key
      (base64-string->u8vector iv)
      (base64-string->u8vector password)))))

;; Legacy function for backward compatibility with old config format
(def (get-password-from-config-legacy key iv password)
  (displayln "WARNING: Using legacy config format with key stored in config file.")
  (displayln "         Please run 'config' command to upgrade to secure key storage.")
  (bytes->string
   (decrypt
    (make-aes-256-ctr-cipher)
    (base64-string->u8vector key)
    (base64-string->u8vector iv)
    (base64-string->u8vector password))))

(def (red txt)
  "Return a red version of txt"
  (format "\x1b;[7;37;41m~a\x1b;[0;37;40m" txt))

(def (open issue)
  "Open Jira issue in browser. Issue ID is validated to prevent shell injection."
  ;; Validate issue ID - only allow alphanumeric, dash, and underscore
  (unless (pregexp-match "^[A-Za-z0-9_-]+$" issue)
    (error "Invalid issue ID format - only alphanumeric characters, dashes, and underscores allowed"))
  (let-hash (load-config)
    (let* ((command (cond-expand
                      (darwin "open")
                      (linux "xdg-open")
                      (bsd "xdg-open")))
           (url (format "~a/browse/~a" .url issue)))
      ;; Use open-process with arguments list instead of shell-command to prevent injection
      (let ((proc (open-process [path: command arguments: [url]])))
        (close-port proc)))))

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
    (set! id-to-user (hash))
    (let ((users (users-hash)))
      (for (user users)
        (let-hash user
          (let ((short (email-short .?emailAddress)))
            (hash-put! user-to-id short .?accountId)
            (hash-put! id-to-user .?accountId short)))))))

(def (convert-ids-to-users str)
  (if (string? str)
    (begin
      (unless (and id-to-user user-to-id)
        (make-user-to-id-hash))
      (let ((re "(?:^|\\s)(?:\\[\\~accountid:)([0-9A-Za-z-:]+)(?:\\])")
            (delim "~accountid:")
            (fmt " @~a")
            (fstr (pregexp-replace* "\\]\\[" str "] [")))
        (hash-interpol re delim fstr id-to-user fmt)))
    ""))

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
  (pregexp-replace* "(\\s|^)@([a-zA-Z0-9]+)" str "\\1\\[\\~\\2\\]"))

(def (default-headers basic)
  [
   ["Accept" :: "*/*"]
   ["Content-type" :: "application/json"]
   ["Authorization" :: basic ]
   ])
