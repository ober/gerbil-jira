;; -*- Gerbil -*-
;;; © ober
;;; Jira client binary

(import
  :gerbil/gambit
  :gerbil/gambit/ports
  :scheme/base
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
(def version "0.05")

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

(def (default-headers basic)
  [
   ["Accept" :: "*/*"]
   ["Content-type" :: "application/json"]
   ["Authorization" :: basic ]
   ])

(def (gettoken)
  (let-hash (load-config)
    (let* ((url (format "~a/rest/api/2/serverinfo" .url))
	   (results (do-get-generic url (default-headers .basic-auth))))
      (displayln results))))

(def (filters)
  (let-hash (load-config)
    (let* ((url (format "~a/rest/api/2/filter" .url))
	   (results (do-get-generic url (default-headers .basic-auth))))
      (displayln results))))

(def (transitions issue)
  (let-hash (load-config)
    (let* ((outs [["id" "name" "toname" "tostate"]])
	   (url (format "~a/rest/api/2/issue/~a/transitions" .url issue))
	   (results (do-get-generic url (default-headers .basic-auth)))
	   (myjson (from-json results)))
      (let-hash myjson
	(for (transition .transitions)
	     (let-hash transition
	       (let-hash .to
		 (set! outs (cons [ ..id ..name .name .description ] outs))))))
      (style-output outs))))

(def (createmetas project basic-auth url)
  (let* ((url (format "~a/rest/api/2/issue/createmeta?projectKeys=~a" url project))
         (results (from-json (jira-get url default-headers basic-auth))))
    results))

(def (transition issue trans)
  (let-hash (load-config)
    (let* ((url (format "~a/rest/api/2/issue/~a/transitions" .url issue))
	   (data (hash
		  ("transition" (hash ("id" trans)))))
	   (results (do-post-generic url (default-headers .basic-auth) (json-object->string data)))
	   (myjson (from-json results)))
      (displayln results))))

(def (watcher-delete issue name)
  (let-hash (load-config)
    (let* ((url (format "~a/rest/api/2/issue/~a/watchers?username=~a" .url issue name))
           (results (do-delete url (default-headers .basic-auth))))
      (displayln results))))

(def (watcher-add issue name)
  (let-hash (load-config)
    (let* ((url (format "~a/rest/api/2/issue/~a/watchers" .url issue))
           (results (do-post url (default-headers .basic-auth) (json-object->string name))))
      (displayln results))))

(def (watchers issue)
  (let-hash (load-config)
    (let* ((out [[ "Name" "Full Name" "Email" "Active?" ]])
           (url (format "~a/rest/api/2/issue/~a/watchers" .url issue))
	   (results (do-get-generic url (default-headers .basic-auth)))
	   (data (from-json results)))
      (when (table? data)
        (let-hash data
          (for (watcher .watchers)
               (let-hash watcher
                 (set! out (cons [ .?name .?displayName .?emailAddress (if .active "Yes" "No") ] out))))))
      (style-output out))))

(def (issuetype type)
  (let-hash (load-config)
    (let* ((url (format "~a/rest/api/2/issuetype/~a" .url type))
           (results (do-get-generic url (default-headers .basic-auth))))
      (displayln results))))

;;com.atlassian.jira.rest.v2.issue.IssueResource.createIssue_post
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

      (let* ((data (hash (fields fields)))
             (results (do-post url (default-headers .basic-auth) (json-object->string data)))
             (myjson (from-json results)))
        (if (table? myjson)
          (let-hash myjson
            .key)
          results)))))

(def (error-print msg (code 2))
  (displayln "Error: " msg)
  (exit code))

(def (converge-template template metas project)
  (if (not (table? template))
    (error-print "Not a table")
    (hash
     (project (get-project-id project metas))
     (summary (interpol-from-env (hash-get template "summary")))
     (issuetype (get-issuetype-id (interpol-from-env (hash-get template "issuetype")) metas))
     (assignee (interpol-from-env (hash-get template "assignee")))
     (priority (interpol-from-env (hash-get template "priority")))
     (labels [(interpol-from-env (hash-get template "labels"))])
     (originalestimate (interpol-from-env (hash-get template "estimate")))
     (description (interpol-from-env (hash-get template "description")))
     (duedate (interpol-from-env (hash-get template "duedate"))))))

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
                    ("duedate" "2018-05-15")))))
           (results (do-post url (default-headers .basic-auth) (json-object->string data)))
           (myjson (from-json results)))
      (displayln results))))

(def (get-project-id name metas)
  (let-hash metas
    (let-hash (car .projects)
      .?id)))

(def (get-issuetype-id name metas)
  (let-hash metas
    (let-hash (car .projects)
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
    (let* ((url (format "~a/rest/api/2/field" .url))
           (results (do-get-generic url (default-headers .basic-auth))))
      (displayln results))))

(def (editmeta issue)
  (let-hash (load-config)
    (let* ((url (format "~a/rest/api/2/issue/~a" .url issue))
           (results (do-get-generic url (default-headers .basic-auth))))
      (display results))))

(def (label issue label)
  (let-hash (load-config)
    (let* ((url (format "~a/rest/api/2/issue/~a" .url issue))
           (data (hash
                  ("fields"
                   (hash
                    ("labels" [ label ])))))
           (results (do-put url (default-headers .basic-auth) (json-object->string data)))
           (myjson (from-json results)))
      (displayln results))))

(def (make-web-safe string)
  (let* ((output (pregexp-replace* " " string "%20")))
    output))

(def (search query)
  (let-hash (load-config)
    (let* ((outs [])
           (query
            (if (or (string-contains query "=")
                    (string-contains query "("))
              (format "~a" query)
              (format "text ~~ '~a'" query)))
           (url (format "~a/rest/api/2/search" .url))
           (data (hash
                  ("jql" query)))
           (results (do-post-generic url (default-headers .basic-auth) (json-object->string data)))
           (myjson (from-json results))
           (issues (let-hash myjson .issues))
           (firms [
                   (when (member "key" .?search-fields) "Key")
                   (when (member "summary" .?search-fields) "Summary")
                   (when (member "priority" .?search-fields) "Priority")
                   (when (member "updated" .?search-fields) "Updated")
                   (when (member "labels" .?search-fields) "Labels")
                   (when (member "status" .?search-fields) "Status")
                   (when (member "assignee" .?search-fields) "Assignee")
                   (when (member "creator" .?search-fields) "Creator")
                   (when (member "reporter" .?search-fields) "Reporter")
                   (when (member "issuetype" .?search-fields) "Issuetype")
                   (when (member "project" .?search-fields) "Project")
                   (when (member "watchers" .?search-fields) "Watchers")
                   (when (member "url" .?search-fields) "Url")
                   ]))

      (set! outs (cons firms outs))
      (for (p issues)
           (let-hash p
             (dp (hash->list .fields))
             (let-hash .fields
               (set! outs (cons
                           [
                            (when (member "key" .?search-fields) ..key)
                            (when (member "summary" .?search-fields) .?summary)
                            (when (member "priority" .?search-fields) (when (table? .?priority) (hash-ref .priority 'name)))
                            (when (member "updated" .?search-fields) (when .?updated (date->custom .updated)))
                            (when (member "labels" .?search-fields) .?labels .?search-fields)
                            (when (member "status" .?search-fields) (when (table? .?status) (red (hash-ref .status 'name))))
                            (when (member "assignee" .?search-fields) (when (table? .?assignee) (hash-ref .assignee 'name)))
                            (when (member "creator" .?search-fields) (when (table? .?creator) (hash-ref .creator 'name)))
                            (when (member "reporter" .?search-fields) (when (table? .?reporter) (hash-ref .reporter 'name)))
                            (when (member "issuetype" .?search-fields) (when (table? .?issuetype) (hash-ref .issuetype 'name)))
                            (when (member "project" .?search-fields) (when (table? .?project) (hash-ref .project 'name)))
                            (when (member "watchers" .?search-fields) (hash-ref .watches 'watchCount))
                            (when (member "url" .?search-fields) (format "~a/browse/~a" ...url ..key))
                             ] outs)))))
      (style-output outs))))


(def (comment issue comment)
  (let-hash (load-config)
    (let* ((url (format "~a/rest/api/2/issue/~a/comment" .url issue))
           (data (hash
                  ("body" comment)))
           (out-js (do-post-generic url (default-headers .basic-auth) (json-object->string data)))
           (results (with-input-from-string out-js read-json)))
      (process-results results))))

(def (process-results results)
  (displayln (hash->list results)))

(def (assign issue user)
  (let-hash (load-config)
    (let* ((url (format "~a/rest/api/2/issue/~a/assignee" .url issue))
           (data (hash
                  ("name" user)))
           (results (do-put url (default-headers .basic-auth) (json-object->string data))))
      (displayln results))))

(def (user pattern)
  (let-hash (load-config)
    (let* ((url (format "~a/rest/api/2/user/search?username=~a" .url pattern))
           (results (do-get-generic url (default-headers .basic-auth)))
           (users (from-json results))
           (outs [[ "User" "Email" "Full Name" "Active?" "Timezone" "Profile" ]]))
      (for (user users)
           (let-hash user
             (set! outs (cons [ .?name .?emailAddress .?displayName .?active .?timeZone .?self ] outs))))
      (style-output outs))))

(def (issue id)
  (let-hash (load-config)
    (let* ((url (format "~a/rest/api/2/issue/~a" .url id))
           (results (do-get-generic url (default-headers .basic-auth)))
           (issue (from-json results)))
      (let-hash issue
        (let-hash .fields
          (displayln "** Summary: " .summary)
          (let-hash .status
            (displayln "** Description: " .description)
            (displayln "** State: " .name))
          (let-hash .priority
            (displayln "** Priority: " .name))
          (let-hash .issuetype
            (displayln "** Issue Type: " .name))
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
              (style-output outs)))
          (displayln "** Comments: ")
          (let-hash .comment
            (for (comment .comments)
                 (let-hash comment
                   (let-hash .author
                     (displayln "*** Comment: " .displayName "  on " ..updated " said:" ))
                   (displayln (pregexp-replace* "*" .body "@")))))
          (let-hash .assignee
            (displayln "** Assignee: " .displayName " " .name " " .emailAddress)))))))

(def (priorities)
  (let-hash (load-config)
    (let* ((url (format "~a/rest/api/2/priority" .url))
           (results (do-get-generic url (default-headers .basic-auth)))
           (priorities (from-json results))
           (outs [[ "Name" "Id" "Description" "Status Color" "Url" "Icon Url" ]]))
      (for (priority priorities)
           (let-hash priority
             (set! outs (cons [ .?name .?id .?description .?statusColor .?self .?iconUrl ] outs))))
      (style-output outs))))

(def (jira-get url headers basic)
  (do-get-generic url (default-headers basic)))

(def (index-summary)
  (let-hash (load-config)
    (let* ((url (format "~a/rest/api/2/index/summary" .url)))
      (displayln (jira-get url default-headers .basic-auth)))))

(def (members project)
  (let-hash (load-config)
    (let* ((url (format "~a/rest/api/2/group/member?groupname=~a&includeInactiveUsers=false" .url project))
           (results (jira-get url default-headers .basic-auth)))
      (display results))))

(def (projects)
  (let-hash (load-config)
    (let* ((url (format "~a/rest/api/2/project" .url))
           (results (do-get-generic url (default-headers .basic-auth)))
           (projects (from-json results))
           (outs [[ "Id" "Key" "Name" ]]))
      (for (project projects)
           (let-hash project
             (set! outs (cons [ .?id .?key .?name .?projectTypeKey ] outs))))
      (style-output outs))))

(def (properties issue)
  (let-hash (load-config)
    (let* ((url (format "~a/rest/api/2/issue/~a/properties" .url issue))
           (results (do-get-generic url (default-headers .basic-auth))))
      (displayln results))))

(def (property issue)
  (let-hash (load-config)
    (let* ((url (format "~a/rest/api/2/issue/~a/properties/sd.initial.field.set" .url issue))
           (results (do-get-generic url (default-headers .basic-auth))))
      (displayln results))))

(def (get-new-ip uri host)
  (pregexp-replace "[0-9]{1,3}\\.[0-9]{1,3}\\.[0-9]{1,3}\\.[0-9]{1,3}" uri (resolve-ipv4 host)))


(def (read-password prompt)
  (let ((password ""))
    (displayln prompt)
    ;;(##tty-mode-set! (current-input-port) #!void #f #!void #!void #!void)
    (set! password (read-line))
    ;;(##tty-mode-set! (current-input-port) #!void #t #!void #!void #!void)
    password))

(def (test-pass)
  (let ((pass (read-password "this is a test:")))
    (displayln "done, and pass is " pass)))

(def (config)
  (let-hash (load-config)
    (let* ((password (read-password "What is your password?: "))
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
      (displayln "-----------------------------------------")
      (displayln "secrets: " secrets)
      (displayln "-----------------------------------------"))))

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
