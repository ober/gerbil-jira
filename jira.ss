;; -*- Gerbil -*-
package: jira
namespace: jira
(export main)

(declare (not optimize-dead-definitions))

(import
  :gerbil/gambit
  :scheme/base
  :std/crypto/cipher
  :std/crypto/etc
  :std/crypto/libcrypto
  :std/format
  :std/generic
  :std/generic/dispatch
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
  :std/text/yaml
  )

(def config-file "~/.jira.yaml")
(import (rename-in :gerbil/gambit/os (current-time builtin-current-time)))
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
   ("create" (hash (description: "create new jira issue") (usage: "create <component> <summary> <description>") (count: 3)))
   ("editmeta" (hash (description: "Get list of fields that can be editied") (usage: "editmeta <issue name>") (count: 1)))
   ("filters" (hash (description: "Get all search filters") (usage: "filters") (count: 0)))
   ("get-issue" (hash (description: "Get Jira Issue") (usage: "jira-get-issue") (count: 1)))
   ("gettoken" (hash (description: "Get Jira TokenVerify account credentials") (usage: "gettoken") (count: 0)))
   ("issue" (hash (description: "Get Jira issue details") (usage: "issue <issue id>") (count: 1)))
   ("label" (hash (description: "label a jira issue") (usage: "label <jira issue> <label>") (count: 2)))
   ("metadata" (hash (description: "Get list of transitions available for issue") (usage: "metadata <issue name>") (count: 1)))
   ("projects" (hash (description: "Get Jira TokenVerify account credentials") (usage: "gettoken") (count: 0)))
   ("q" (hash (description: "Execute one of your stored queries in your ~/.jira.yaml" ) (usage: "q <query name>") (count: 1)))
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
    (let* ((url (format "~a/rest/api/2/issue/~a/transitions" .url issue))
	   (results (do-get-generic url (default-headers .basic-auth)))
	   (myjson (from-json results)))
      (displayln (type-of results))
      (displayln "|id|name|to name|to state|")
      (displayln "|--|--|")
      (let-hash myjson
	(for-each
	  (lambda (t)
	    (let-hash t
	      (let-hash .to
		(displayln "|" ..id
			   "|" ..name
			   "|" .name
			   "|" .description
			   "|"))))
	  .transitions)))))

(def (metadata issue)
  (let-hash (load-config)
    (let* ((url (format "~a/rest/api/2/issue/~a/editmeta" .url issue))
	   (results (do-get-generic url (default-headers .basic-auth)))
	   (myjson (from-json results)))
      (displayln results))))

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
	  (for-each
	    (lambda (c)
	      (displayln (stringify-hash c)))
	    .components))))))

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

(def (create component summary description)
  (let-hash (load-config)
    (let* ((url (format "~a/rest/api/2/issue" .url))
	   (data (hash
		  ("fields"
		   (hash
		    ("project" (hash ("id" "10071")))
		    ("summary" summary)
		    ("issuetype" (hash ("id" "3"))) ;; task == 3
		    ("assignee" (hash ("name" .user)))
		    ("components" [ (hash ("name" component)) ])
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
    (let* ((query
	    (if (or (string-contains query "=")
		    (string-contains query "("))
	      (format "~a" query)
	      (format "text ~~ '~a'" query)))
	   (url (format "~a/rest/api/2/search" .url))
	   (data (hash
		  ("jql" query)))
	   (results (do-post-generic url (default-headers .basic-auth) (json-object->string data)))
	   (style (or .?style "org-mode"))
	   (myjson (with-input-from-string results read-json))
	   (firms [ "Key"
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
		    ]))

      (print-header style firms)
      (for-each
	(lambda (p)
    	  (let-hash p
	    (dp (hash->list .fields))
    	    (let-hash .fields
	      (print-row style
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
			   ]))))
	(hash-ref myjson 'issues)))))

(def (print-header style header)
  (let-hash (load-config)
    (cond
     ((string=? style "org-mode")
      (begin
	(displayln "|" (string-join header "|") "|")
	(displayln "|-|")))
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
    (for-each
      (lambda (datum)
	(printf "|~a" datum))
      data)
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
	   (myjson (from-json results)))
      (displayln "|User | Email| Full Name | Active | Timezone| Profile |")
      (displayln "|-|")
      (for-each
	(lambda (n)
	  (let-hash n
	    (displayln "|" .name
		       "|" .emailAddress
		       "|" .displayName
		       "|" .active
		       "|" .timeZone
		       "|" .self "|")))
	myjson))))

(def (issue id)
  (let-hash (load-config)
    (let* ((url (format "~a/rest/api/2/issue/~a" .url id))
	   (results (do-get-generic url (default-headers .basic-auth)))
	   (myjson (from-json results)))
      (let-hash myjson
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
	  (if .?subtasks
	    (begin
	      (displayln "|ID|Summary | Status | Priority|")
	      (displayln "|-|")
	      (for-each
		(lambda (s)
		  (let-hash s
		    (let-hash .fields
		      (displayln "|" ..?key
				 "|" .?summary
				 "|" (hash-ref .status 'name)
				 "|" (hash-ref .priority 'name)
				 "|"))))
		.subtasks)))
	  (displayln "** Comments: ")
	  (let-hash .comment
	    (for-each
	      (lambda (c)
		(let-hash c
		  (let-hash .author
		    (displayln "*** Comment: " .displayName "  on " ..updated " said:" ))
		  (displayln (pregexp-replace* "*" .body "@"))))
	      .comments))

	  (let-hash .assignee
	    (displayln "** Assignee: " .displayName " " .name " " .emailAddress)))))))

(def (projects)
  (let-hash (load-config)
    (let* ((url (format "~a/rest/api/2/project" .url))
	   (results (do-get-generic url (default-headers .basic-auth)))
	   (myjson (from-json results)))
      (displayln "|id|key|name|")
      (displayln "|-|")
      (for-each
	(lambda (p)
	  (let-hash p
	    (displayln "|" .id
		       "|" .key
		       "|" .name
		       "|" .projectTypeKey
		       "|")))
	myjson))))

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
  (displayln "Usage: jira <verb>")
  (displayln "Verbs:")
  (for-each
    (lambda (k)
      (displayln (format "~a: ~a" k (hash-get (hash-get interactives k) description:))))
    (sort! (hash-keys interactives) string<?))
  (exit 2))

(def (config)
  (let-hash (load-config)
    (displayln "What is your password?: ")
    (let* ((password (read-line (current-input-port)))
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
