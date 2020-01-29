; -*- Gerbil -*-
;;; © ober
;;; Jira client binary

(import
  :gerbil/gambit
  :gerbil/gambit/ports
  :std/crypto/cipher
  :std/crypto/etc
  :std/crypto/libcrypto
  :std/db/dbi
  :std/debug/heap
  :std/iter
  :std/error
  :std/format
  :std/generic
  :std/generic/dispatch
  :std/misc/channel
  :std/misc/list
  :std/misc/ports
  :std/net/address
  :std/net/request
  :std/net/uri
  :std/pregexp
  :std/srfi/1
  :std/srfi/13
  :std/srfi/19
  :std/srfi/95
  :std/sugar
  :std/text/base64
  :std/text/json
  :std/text/utf8
  :std/text/yaml
  :std/text/zlib
  :ober/oberlib
  :std/xml/ssax)

(export #t)

(declare (not optimize-dead-definitions))
(def version "0.09")

(def config-file "~/.jira.yaml")
(import (rename-in :gerbil/gambit/os (current-time builtin-current-time)))
(import (rename-in :gerbil/gambit/os (time mytime)))
(def program-name "jira")

(def good-ips (hash))

(def (load-config)
  (let ((config (hash)))
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
        (present-item body)))))

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

(def (createmetas project basic-auth url)
  (let (url (format "~a/rest/api/2/issue/createmeta?projectKeys=~a" url project))
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

(def (watcher-delete issue name)
  (let-hash (load-config)
    (let ((url (format "~a/rest/api/2/issue/~a/watchers?username=~a" .url issue name)))
      (with ([status body] (rest-call 'delete url (default-headers .basic-auth)))
        (present-item body)))))

(def (watcher-add issue name)
  (let-hash (load-config)
    (let ((url (format "~a/rest/api/2/issue/~a/watchers" .url issue)))
      (with ([status body] (rest-call 'post url (default-headers .basic-auth) (json-object->string name)))
        (unless status
          (error body))
        (present-item body)))))

(def (watchers issue)
  (let-hash (load-config)
    (let ((out [[ "Name" "Full Name" "Email" "Active?" ]])
          (url (format "~a/rest/api/2/issue/~a/watchers" .url issue)))
      (with ([status body] (rest-call 'get url (default-headers .basic-auth)))
        (unless status
          (error body))
        (let-hash body
          (for (watcher .watchers)
            (let-hash watcher
              (set! out (cons [ .?name .?displayName .?emailAddress (if .active "Yes" "No") ] out))))))
      (style-output out .style))))

(def (issuetype type)
  (let-hash (load-config)
    (let ((url (format "~a/rest/api/2/issuetype/~a" .url type)))
      (with ([status body] (rest-call 'get url (default-headers .basic-auth)))
        (unless status
          (error body))
        (present-item body)))))

(def (create-issue project summary issuetype assignee priority labels originalestimate description duedate parent)
  (displayln "proj: " project " sum: " summary " issuetype: " issuetype " assignee: " assignee " priority: " priority " labels: " labels " estimate: " originalestimate " description: " description " duedate: " duedate " parent: " parent)
  (let-hash (load-config)
    (let* ((url (format "~a/rest/api/2/issue" .url))
           (fields (hash
                    ("project" (hash ("id" project)))
                    ("summary" summary)
                    ("issuetype" (hash ("id" issuetype)))
                    ("assignee" (hash ("name" assignee)))
                    ;;	    ("components" [ (hash ("name" component)) ])
                    ("priority" (hash ("name" priority)))
                    ("labels" [])
                    ("timetracking" (hash
                                     ("originalEstimate" originalestimate)))
                    ("description" description)
                    ("duedate" duedate))))

      (when parent
        (hash-put! fields "parent" (hash ("id" parent))))

      (when (= (string->number issuetype) 5)
        (hash-put! fields "customfield_10496" summary))

      (with ([status body] (rest-call 'post url (default-headers .basic-auth) (json-object->string (hash (fields fields)))))
        (unless status
          (error body))
        (if (table? body)
          (let-hash body
            .key)
          body)))))

(def (error-print msg (code 2))
  (displayln "Error: " msg)
  (exit code))

(def (converge-template template metas project)
  (if (not (table? template))
    (error-print "Not a table")
    (hash
     (assignee (interpol-from-env (hash-get template "assignee")))
     (description (interpol-from-env (hash-get template "description")))
     (duedate (interpol-from-env (hash-get template "duedate")))
     (issuetype (get-issuetype-id (interpol-from-env (hash-get template "issuetype")) metas))
     (labels [(interpol-from-env (hash-get template "labels"))])
     (originalestimate (interpol-from-env (hash-get template "estimate")))
     (priority (interpol-from-env (hash-get template "priority")))
     (project (interpol-from-env (hash-get template "project")))
     (summary (interpol-from-env (hash-get template "summary")))
     )))

(def (execute-template template metas project parent)
  (if (not (table? template))
    (begin
      (displayln "Error: execute-template passed non-table :"  template)
      (exit 2)))
  (let ((converged (converge-template template metas project)))
    (let-hash converged
      (let ((parent2 (create-issue .project
                                   .summary
                                   .issuetype
                                   .assignee
                                   .priority
                                   .labels
                                   .originalestimate
                                   .description
                                   .duedate
                                   parent))
            (subtasks (hash-get template "subtasks")))
        (when subtasks
          (for (subtask subtasks)
            (execute-template subtask metas projects parent2)))
        (unless parent
          (displayln "Primary issue: " parent2))))))

(def (run creation)
  (let-hash (load-config)
    (let ((metas (createmetas .project-key .basic-auth .url)))
      (if .?creations
        (let ((creature (hash-get .creations creation)))
          (if creature
            (execute-template creature metas .project-key #f)
            (begin
              (displayln "Error: could not find an entry for " creation " in your ~/.jira.yaml under the creations block")
              (exit 2))))
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
    (let-hash .projects
      (let ((id 0))
        (for (issuetype .issuetypes)
          (let-hash issuetype
            (when (string=? .name name)
              (set! id .id))))
        id))))

(def (parse-metas)
  (let-hash (load-config)
    (let ((metas (createmetas .project-key .basic-auth .url)))
      (let-hash metas
        (let-hash (car .projects)
          (for (its .issuetypes)
            (displayln (hash->list its))))))))

(def (fields)
  (let-hash (load-config)
    (let ((url (format "~a/rest/api/2/field" .url)))
      (with ([status body] (rest-call 'get url (default-headers .basic-auth)))
        (unless status
          (error body))
        (present-item body)))))

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
      (with ([status body] (rest-call 'pput url (default-headers .basic-auth) (json-object->string data)))
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
           (data (hash
                  ("jql" query)))

           (headers (if (and sf
                             (list? sf)
                             (length>n? sf 1))
                      sf
                      df)))

      (with ([ status body ] (rest-call 'post url (default-headers .basic-auth) (json-object->string data)))
        (unless status
          (error body))
        (if (table? body)
          (let-hash body
            (set! outs (cons headers outs))
            (for (issue .issues)
              (let-hash issue
                (dp (hash->list .fields))
                (let-hash .fields
                  (set! outs
                    (cons
                     (filter-row-hash
                      (hash
                       ("key" ..key)
                       ("description" .?description)
                       ("summary" .?summary)
                       ("priority" (when (table? .?priority) (hash-ref .?priority 'name)))
                       ("updated" (when .?updated (date->custom .updated)))
                       ("labels" .?labels)
                       ("status" (when (table? .?status) (hash-ref .status 'name)))
                       ("assignee" (when (table? .?assignee) (hash-ref .assignee 'name)))
                       ("creator" (when (table? .?creator) (hash-ref .creator 'name)))
                       ("reporter" (when (table? .?reporter) (hash-ref .reporter 'name)))
                       ("issuetype" (when (table? .?issuetype) (hash-ref .issuetype 'name)))
                       ("project" (when (table? .?project) (hash-ref .project 'name)))
                       ("watchers" (hash-ref .watches 'watchCount))
                       ("url" (format "~a/browse/~a" ....url ..key))) headers) outs)))))
            (style-output outs ..style)))))))

(def (comment issue comment)
  (let-hash (load-config)
    (let* ((url (format "~a/rest/api/2/issue/~a/comment" .url issue))
           (fixed-comment (convert-names comment))
           (data (hash
                  ("body" fixed-comment))))
      (with ([status body] (rest-call 'post url (default-headers .basic-auth) (json-object->string data)))
        (unless status
          (error body))
        (present-item body)))))

(def (assign issue user)
  (let-hash (load-config)
    (let ((url (format "~a/rest/api/2/issue/~a/assignee" .url issue))
          (data (hash ("name" user))))
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

(def (issue id)
  (let-hash (load-config)
    (let ((url (format "~a/rest/api/2/issue/~a" .url id)))
      (with ([status body] (rest-call 'get url (default-headers .basic-auth)))
        (unless status
          (error body))
        (when
            (table? body)
          (let-hash body
            (let-hash .fields
              (displayln "** Summary: " .summary)
              (when .?status (let-hash .status (displayln "** Description: " .description) (displayln "** State: " .name)))
              (when .?priority (let-hash .priority (displayln "** Priority: " .name)))
              (when .?issuetype (let-hash .issuetype   (displayln "** Issue Type: " .name)))
              (displayln "** Description: " .description)
              (displayln "** Summary: " .summary)
              (displayln "** Last Viewed: " .lastViewed)
              (displayln "** Created: " .created)

              (let-hash .status (displayln "** Status: " .name))
              (let-hash .reporter (displayln "** Reporter: " .displayName " " .name " " .emailAddress))
              (let-hash .project (displayln "** Project: " .name))
              (let-hash .watches (displayln "** Watch Count: " .watchCount))
              (let-hash .creator (displayln "** Creator: " .displayName " " .name " " .emailAddress))
              (displayln "** Subtasks: ")
              (when .?subtasks
                (let ((outs [[ "Id" "Summary" "Status" "Priority" ]]))
                  (for (subtask .subtasks)
                    (let-hash subtask
                      (let-hash .fields
                        (set! outs (cons [ ..?key .?summary (hash-ref .status 'name)  (hash-ref .priority 'name) ] outs)))))
                  (style-output outs (or .?style "org-mode"))))
              (displayln "** Comments: ")
              (let-hash .comment
                (for (comment .comments)
                  (let-hash comment
                    (let-hash .author
                      (displayln "*** Comment: " .displayName "  on " ..updated " said:" ))
                    (displayln (pregexp-replace* "*" .body "@")))))
              (let-hash .assignee
                (displayln "** Assignee: " .displayName " " .name " " .emailAddress)))))))))

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
      (with ([status body] (rest-call 'get url default-headers .basic-auth))
        (unless status
          (error body))
        (present-item body)))))

(def (members project)
  (let-hash (load-config)
    (let* ((url (format "~a/rest/api/2/group/member?groupname=~a&includeInactiveUsers=false" .url project)))
      (with ([status body] (rest-call 'get url (default-headers .basic-auth)))
        (unless status
          (error body))
        (present-item body)))))

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
    (let ((url (format "~a/rest/api/2/issue/~a/properties" .url issue)))
      (with ([status body] (rest-call 'get url (default-headers .basic-auth)))
        (unless status
          (error body))
        (present-item body)))))

(def (property issue)
  (let-hash (load-config)
    (let ((url (format "~a/rest/api/2/issue/~a/properties/sd.initial.field.set" .url issue)))
      (with ([status body] (rest-call 'get url (default-headers .basic-auth)))
        (unless status
          (error body))
        (present-item body)))))

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

(def (convert-names str)
  (let ((results ""))
    (pregexp-replace* "(\\s)@([a-zA-Z0-9]+)" str "\\1\\[\\~\\2\\]")))

(def (default-headers basic)
  [
   ["Accept" :: "*/*"]
   ["Content-type" :: "application/json"]
   ["Authorization" :: basic ]
   ])
