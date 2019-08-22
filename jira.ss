; -*- Gerbil -*-
package: jira
namespace: jira
(export main)

(declare (not optimize-dead-definitions))
(def version "0.03")

(import
  :gerbil/gambit
  :scheme/base
  :std/crypto/cipher
  :std/crypto/etc
  :std/crypto/libcrypto
  :std/format
  :std/generic
  :std/generic/dispatch
  :std/iter
  :std/misc/channel
  :std/misc/ports
  :std/net/address
  :std/net/request
  :std/pregexp
  :std/srfi/13
  :std/srfi/19
  :std/srfi/95
  :std/sugar
  :std/text/base64
  :std/text/json
  :std/text/utf8
  :std/text/yaml)

(def config-file "~/.jira.yaml")
(import (rename-in :gerbil/gambit/os (current-time builtin-current-time)))
(import (rename-in :gerbil/gambit/os (time mytime)))
(def program-name "jira")
(def DEBUG (getenv "DEBUG" #f))
(def (dp msg)
  (when DEBUG
    (displayln msg)))

(def good-ips (hash))

(def interactives
  (hash
   ("assign" (hash (description: "Assign Issue to user") (usage: "assign <issue id> <user>") (count: 2)))
   ("comment" (hash (description: "Add comment to Jira issue") (usage: "comment <issue> <comment>") (count: 2)))
   ("config" (hash (description: "Setup your user and password in the config encrypted") (usage: "config") (count: 0)))
   ("parse-metas" (hash (description: "Setup your user and password in the config encrypted") (usage: "config") (count: 0)))
   ("create" (hash (description: "create new jira issue") (usage: "create <summary> <description>") (count: 2)))
   ("fields" (hash (description: "Return all fields") (usage: "fields") (count: 0)))
   ("filters" (hash (description: "Get all search filters") (usage: "filters") (count: 0)))
   ("get-issue" (hash (description: "Get Jira Issue") (usage: "jira-get-issue") (count: 1)))
   ("get-issuetype-id" (hash (description: "Get Jira issuetype id from name") (usage: "get-issuetype-id <issuetype name>") (count: 1)))
   ("gettoken" (hash (description: "Get Jira TokenVerify account credentials") (usage: "gettoken") (count: 0)))
   ("issue" (hash (description: "Get Jira issue details") (usage: "issue <issue id>") (count: 1)))
   ("index-summary" (hash (description: "Get Index Summary Details") (usage: "index-summary") (count: 0)))
   ("issuetype" (hash (description: "Get information on issuetype") (usage: "issuetype <issuetype id>") (count: 1)))
   ("label" (hash (description: "label a jira issue") (usage: "label <jira issue> <label>") (count: 2)))
   ("metadata" (hash (description: "Get definitions of fields available for issue.") (usage: "metadata <issue name>") (count: 1)))
   ("members" (hash (description: "Get list of members of a given project.") (usage: "members <Project Name>") (count: 1)))
   ("projects" (hash (description: "List all projects") (usage: "projects") (count: 0)))
   ("properties" (hash (description: "Fetch all properties available for issue") (usage: "properties <issue id>") (count: 1)))
   ("property" (hash (description: "Fetch all properties available for issue") (usage: "properties <issue id>") (count: 1)))
   ("priorities" (hash (description: "List priorities available for issues") (usage: "priorities") (count: 0)))
   ("q" (hash (description: "Execute one of your stored queries in your ~/.jira.yaml" ) (usage: "q <query name>") (count: 1)))
   ("run" (hash (description: "Execute one of your stored creations in your ~/.jira.yaml" ) (usage: "run <creation name>") (count: 1)))
   ("search" (hash (description: "Search for issues matching string") (usage: "search <query string>") (count: 1)))
   ("transition" (hash (description: "Transition issue to new state.") (usage: "transition <issue name> <transition id>") (count: 2)))
   ("transitions" (hash (description: "Get list of transitions available for issue") (usage: "transitions <issue name>") (count: 1)))
   ))

(def (main . args)
  (if (null? args)
    (usage))
  (let* ((argc (length args))
	 (verb (car args))
	 (args2 (cdr args)))
    (unless (hash-key? interactives verb)
      (usage))
    (let* ((info (hash-get interactives verb))
	   (count (hash-get info count:)))
      (unless count
	(set! count 0))
      (unless (= (length args2) count)
	(usage-verb verb))
      (apply (eval (string->symbol (string-append "jira#" verb))) args2))))

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

(def (do-get-generic uri headers)
  (let* ((reply (http-get uri
			  headers: headers))
	 (status (request-status reply))
	 (text (request-text reply)))
    (print-curl "get" uri "" "")
    (if (success? status)
      text
      (displayln (format "Error: got ~a on request. text: ~a~%" status text)))))

(def (from-json json)
  (try
   (with-input-from-string json read-json)
   (catch (e)
     (displayln "error parsing json " e))))

(def (do-put uri headers data)
  (dp (print-curl "put" uri headers data))
  (let* ((reply (http-put uri
			  headers: headers
			  data: data))
	 (status (request-status reply))
	 (text (request-text reply)))

    (if (success? status)
      (displayln text)
      (displayln (format "Failure on put. Status:~a Text:~a~%" status text)))))

(def (do-post-generic uri headers data)
  (try
   (let* ((reply (http-post uri
			    headers: headers
			    data: data))
	  (status (request-status reply))
	  (text (request-text reply)))
     (dp (print-curl "post" uri headers data))
     (if (success? status)
       text
       (displayln (format "Error: Failure on a post. got ~a text: ~a~%" status text))))
   (catch (e)
     (display-exception e))))

(def (do-post uri headers data)
  (dp (print-curl "post" uri headers data))
  (try
   (let* ((reply (http-post uri
			    headers: headers
			    data: data))
	  (status (request-status reply))
	  (text (request-text reply)))

     (if (success? status)
       text
       (displayln (format "Failure on post. Status:~a Text:~a~%" status text))))
   (catch (e)
     (display-exception e))))

(def (success? status)
  (and (>= status 200) (<= status 299)))

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
  (displayln "createmetas project: " project)
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

(def (get-issue issue)
  (let-hash (load-config)
    (let* ((url (format "~a/rest/api/2/issue/~a" .url issue))
	   (results (do-get-generic url (default-headers .basic-auth)))
	   (myjson (from-json results)))
      (displayln "|id|assignee email|assignee name|reporter|watchers|description|summary|priority|status|")
      (displayln "|--|--|")

      (let-hash myjson
	(let-hash .fields
	  (for (component .components)
	       (displayln (stringify-hash component))))))))

(def (stringify-hash h)
  (let ((results []))
    (if (table? h)
      (begin
	(hash-for-each
	 (lambda (k v)
	   (set! results (append results (list (format " ~a->" k) (format "~a   " v)))))
	 h)
	(append-strings results))
      "N/A")))

(def (issuetype type)
  (let-hash (load-config)
    (let* ((url (format "~a/rest/api/2/issuetype/~a" .url type))
           (results (do-get-generic url (default-headers .basic-auth))))
      (displayln results))))

;;com.atlassian.jira.rest.v2.issue.IssueResource.createIssue_post
(def (create-issue project summary issuetype assignee priority labels originalestimate description duedate)
  (let-hash (load-config)
    (let* ((url (format "~a/rest/api/2/issue" .url))
	   (data (hash
		  ("fields"
		   (hash
		    ("project" (hash ("id" project)))
		    ("summary" summary)
		    ("issuetype" (hash ("id" issuetype))) ;; task == 3
		    ("assignee" (hash ("name" assignee)))
		    ;;	    ("components" [ (hash ("name" component)) ])
		    ("priority" (hash ("name" priority)))
		    ("labels" [ labels ])
		    ("timetracking" (hash
				     ("originalEstimate" originalestimate)))
		    ("description" description)
		    ("duedate" duedate)))))
           ;;	   (results (do-post url (default-headers .basic-auth) (json-object->string data)))
           ;;	   (myjson (from-json results)))
           )
      (displayln "not running query"))))
;;      (displayln results))))

(def (remove-bad-matches vars omit)
  (let ((goodies []))
    (for (var vars)
         (unless (string-contains var omit)
           (set! goodies (flatten (cons var goodies)))))
    (reverse goodies)))

(def (interpol-from-env str)
  (let* ((ruby (pregexp "#\\{([a-zA-Z0-9]*)\\}"))
         (vars (remove-bad-matches (match-regexp ruby str) "#"))
         (newstr (pregexp-replace* ruby str "~a"))
         (set-vars []))

    (for (var vars)
         (let ((val (getenv var #f)))
           (if (not val)
             (begin
               (displayln "Error: Variable " var " is used in the template, but not defined in the environment")
               (exit 2))
             (set! set-vars (cons val set-vars)))))

    (apply format newstr (reverse set-vars))))

(def (match-regexp pat str . opt-args)
  "Like pregexp-match but for all matches til end of str"
  (let ((n (string-length str))
        (ix-prs []))
    (let lp ((start 0))
      (let* ((pp (pregexp-match-positions pat str start n))
             (ix-pr (pregexp-match pat str start n)))
        (if ix-pr
          (let ((pos (+ 1 (cdar pp))))
            (set! ix-prs (flatten (cons ix-pr ix-prs)))
            (if (< pos n)
              (lp pos)
              ix-prs))
          (reverse ix-prs))))))

(def (error-print msg (code 2))
  (displayln "Error: " msg)
  (exit code))

(def (converge-template template metas)
  (if (not (table? template))
    (error-print "Not a table")
    (let-hash template
      (hash
       (project (get-project-id .project-key metas))
       (summary (interpol-from-env .summary))

       ))))
;;       (issuetype

(def (interpolate-vars template)
  (displayln "template is " (hash->list template))
  (hash (project [])
        (summary "some big summary")
        (issuetype 3)
        (assignee "me")
        (priority 2)
        (labels [ "big issue"])
        (originalestimate 10)
        (description "big description here")
        (duedate "08-21-1971")))

(def (execute-template template metas)
  (if (not (table? template))
    (begin
      (displayln "Error: execute-template passed non-table :"  template)
      (exit 2)))
  (let ((converged (converge-template template metas)))
    (let-hash converged
      (create-issue .project
                    .summary
                    .issuetype
                    .assignee
                    .priority
                    .labels
                    .originalestimate
                    .description
                    .duedate))))

(def (run creation)
  (let-hash (load-config)
    (let ((metas (createmetas .project-key .basic-auth .url)))
      (if .?creations
        (let ((creature (hash-get .creations creation)))
          (if creature
            (execute-template creature metas)
            (begin
              (displayln "Error: could not find an entry for " creation " in your ~/.jira.yaml under the creations block")
              (exit 2))))
        (displayln "Error: no create templates defined in ~/.jira.yaml under creations")))))

(def (create summary description)
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
    .id))

(def (get-issuetype-id name metas)
  (let-hash metas
    (let ((id 0))
      (for (project .projects)
           (let-hash project
             (when (string=? .name name)
               (set! id .id))))
      id)))

(def (parse-metas)
  (let-hash (load-config)
    (let ((metas (createmetas .project-key .basic-auth .url)))
       (let-hash metas
         (for (project .projects)
              (displayln (stringify-hash project)))))))

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
           (firms ;;(or .?search-fields
            [ "Key"
              "Summary"
              "Priority"
              "Updated"
              "Labels"
              "Status"
              "Assignee"
              "Creator"
              "Reporter"
              "Issuetype "
              "Project"
              "watchers"
              "Url"
              ] )) ;;)
      (set! outs (cons firms outs))
      (for (p issues)
           (let-hash p
             (dp (hash->list .fields))
             (let-hash .fields
               (set! outs (cons
                           [ ..key
                             .?summary
                             (when (table? .?priority) (hash-ref .priority 'name))
                             (when .?updated (date->custom .updated))
                             .?labels
                             (when (table? .?status) (hash-ref .status 'name))
                             (when (table? .?assignee) (hash-ref .assignee 'name))
                             (when (table? .?creator) (hash-ref .creator 'name))
                             (when (table? .?reporter) (hash-ref .reporter 'name))
                             (when (table? .?issuetype) (hash-ref .issuetype 'name))
                             (when (table? .?project) (hash-ref .project 'name))
                             (hash-ref .watches 'watchCount)
                             (format "~a/browse/~a" ...url ..key)
                             ] outs)))))
      (style-output outs))))

(def (format-string-size string size)
  (unless (string? string)
    (set! string (format "~a" string)))
  (let* ((string (string-trim-both string))
         (our-size (string-length string))
         (delta (if (> size our-size)
                  (- size our-size)
                  0)))
    ;;    (displayln "fss: delta: " delta " string: " string " our-size: " our-size " size: " size)
    (format " ~a~a" string (make-string delta #\space))))

(def (style-output infos)
  (let-hash (load-config)
    (when (list? infos)
      (let* ((sizes (hash))
             (data (reverse infos))
             (header (car data))
             (rows (cdr data)))
        (for (head header)
             (unless (string? head) (displayln "head is not string: " head) (exit 2))
             (hash-put! sizes head (string-length head)))
        (for (row rows)
             (let (count 0)
               (for (column row)
                    (let* ((col-name (nth count header))
                           (current-size (hash-ref sizes col-name))
                           (this-size (if (string? column) (string-length column) (string-length (format "~a" column)))))
                      (when (> this-size current-size)
                        (hash-put! sizes col-name this-size))
                      ;;		      (displayln "colname: " col-name " col: " count " current-size: " current-size " this-size: " this-size " column: " column)
                      (set! count (1+ count))))))

        (for (head header)
             (display (format "| ~a" (format-string-size head (hash-get sizes head)))))

        ;; print header
        (displayln "|")
        (let ((count 0))
          (for (head header)
               (let ((sep (if (= count 0) "|" "+")))
                 (display (format "~a~a" sep (make-string (+ 2 (hash-get sizes (nth count header))) #\-))))
               (set! count (1+ count))))
        (displayln "|")

        (for (row rows)
             (let (count 0)
               (for (col row)
                    (display (format "|~a " (format-string-size col (hash-ref sizes (nth count header)))))
                    (set! count (1+ count))))
             (displayln "|"))
        ))))

(def (print-header style header)
  (let-hash (load-config)
    (cond
     ((string=? style "org-mode")
      (displayln "| " (string-join header " | ") " |")
      (displayln "|-|"))
     (else
      (displayln "Unknown format: " style)))))

(def (print-row style data)
  (if (list? data)
    (cond
     ((string=? style "org-mode")
      (org-mode-print-row data))
     (else
      (displayln "Unknown format! " style)))))

(def (org-mode-print-row data)
  (when (list? data)
    (for (datum data)
         (printf "| ~a " datum))
    (displayln "|")))

(def (flatten x)
  (cond ((null? x) [])
        ((pair? x) (append (flatten (car x)) (flatten (cdr x))))
        (else (list x))))

(def (date->custom dt)
  (date->string (string->date dt "~Y-~m-~dT~H:~M:~S~z") "~a ~b ~d ~Y"))

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
           (users (from-json results)))
      (displayln "|User | Email| Full Name | Active | Timezone| Profile |")
      (displayln "|-|")
      (for (user users)
           (let-hash user
             (displayln "|" .name
                        "|" .emailAddress
                        "|" .displayName
                        "|" .active
                        "|" .timeZone
                        "|" .self "|"))))))

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
            (displayln "|ID|Summary | Status | Priority|")
            (displayln "|-|")
            (for (subtask .subtasks)
                 (let-hash subtask
                   (let-hash .fields
                     (displayln "|" ..?key
                                "|" .?summary
                                "|" (hash-ref .status 'name)
                                "|" (hash-ref .priority 'name)
                                "|")))))

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
           (priorities (from-json results)))
      (displayln "|Name|Id|Description|Status Color| Url|Icon Url|")
      (displayln "|-|")
      (for (priority priorities)
           (let-hash priority
             (displayln "|" .?name
                        "|" .?id
                        "|" .?description
                        "|" .?statusColor
                        "|" .?self
                        "|" .?iconUrl))))))


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
           (projects (from-json results)))
      (displayln "|id|key|name|")
      (displayln "|-|")
      (for (project projects)
           (let-hash project
             (displayln "|" .id
                        "|" .key
                        "|" .name
                        "|" .projectTypeKey
                        "|"))))))

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

(def (print-curl type uri headers data)
  ;;(displayln headers)
  (let ((heads "Content-type: application/json")
        (do-curl (getenv "DEBUG" #f)))
    (when do-curl
      (cond
       ((string=? type "get")
        (if (string=? "" data)
          (displayln (format "curl -X GET -H \'~a\' ~a" heads uri))
          (displayln (format "curl -X GET -H \'~a\' -d \'~a\' ~a" heads data uri))))
       ((string=? type "put")
        (displayln (format "curl -X PUT -H \'~a\' -d \'~a\' ~a" heads data uri)))
       ((string=? type "post")
        (displayln (format "curl -X POST -H \'~a\' -d \'~a\' ~a" heads data uri)))
       ((string=? type "delete")
        (displayln (format "curl -X DELETE -H \'~a\' -d \'~a\' ~a" heads data uri)))
       (else
        (displayln "unknown format " type))))))

(def (get-new-ip uri host)
  (pregexp-replace "[0-9]{1,3}\\.[0-9]{1,3}\\.[0-9]{1,3}\\.[0-9]{1,3}" uri (resolve-ipv4 host)))

(def (resolve-ipv4 host)
  (if (hash-key? good-ips host)
    (hash-get good-ips host)
    (let* ((host-info (host-info-addresses (host-info host))))
      (dp (format "host-info: ~a type:~a" host-info (type-of host-info)))
      (ip4-address->string
       (car host-info)))))

(def (make-basic-auth user password)
  (format "Basic ~a"
          (base64-encode
           (string->utf8 (format "~a:~a" user password)))))

(def (usage-verb verb)
  (let ((howto (hash-get interactives verb)))
    (displayln "Wrong number of arguments. Usage is:")
    (displayln program-name " " (hash-get howto usage:))
    (exit 2)))

(def (usage)
  (displayln (format "Jira: version ~a" version))
  (displayln "Usage: jira <verb>")
  (displayln "Verbs:")
  (for (verb (sort! (hash-keys interactives) string<?))
       (displayln (format "~a: ~a" verb (hash-get (hash-get interactives verb) description:))))
  (exit 2))

(def (read-password prompt)
  (let ((password ""))
    (displayln prompt)
    (##tty-mode-set! (current-input-port) #!void #f #!void #!void #!void)
    (set! password (read-line))
    (##tty-mode-set! (current-input-port) #!void #t #!void #!void #!void)
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

(def (nth n l)
  "Implement nth for gerbil. fetch n argument from list"
  (if (or (> n (length l)) (< n 0))
    (error "Index out of bounds.")
    (if (eq? n 0)
      (car l)
      (nth (- n 1) (cdr l)))))
