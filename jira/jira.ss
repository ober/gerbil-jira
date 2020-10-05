;;; -*- Gerbil -*-
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
  :std/xml/ssax
  :ober/oberlib
  :ober/jira/client)

(export main)

(declare (not optimize-dead-definitions))

(def program-name "jira")

(def interactives
  (hash
   ("assign" (hash (description: "Assign Issue to user") (usage: "assign <issue id> <user>") (count: 2)))
   ("comment" (hash (description: "Add comment to Jira issue") (usage: "comment <issue> <comment>") (count: 2)))
   ("config" (hash (description: "Setup your user and password in the config encrypted") (usage: "config") (count: 0)))
   ("parse-metas" (hash (description: "Setup your user and password in the config encrypted") (usage: "config") (count: 0)))
   ("create-issue" (hash (description: "create new jira issue") (usage: "create <project> <summary> <description>") (count: 3)))
   ("create" (hash (description: "create new jira issue") (usage: "create <project> <summary> <description>") (count: 3)))
   ("fields" (hash (description: "Return all fields") (usage: "fields") (count: 0)))
   ("filters" (hash (description: "Get all search filters") (usage: "filters") (count: 0)))
   ("get-issuetype-id" (hash (description: "Get Jira issuetype id from name") (usage: "get-issuetype-id <issuetype name>") (count: 1)))
   ("gettoken" (hash (description: "Get Jira TokenVerify account credentials") (usage: "gettoken") (count: 0)))
   ("issue" (hash (description: "Get Jira issue details") (usage: "issue <issue id>") (count: 1)))
   ("index-summary" (hash (description: "Get Index Summary Details") (usage: "index-summary") (count: 0)))
   ("issuetype" (hash (description: "Get information on issuetype") (usage: "issuetype <issuetype id>") (count: 1)))
   ("watchers" (hash (description: "Get Watchers on issue") (usage: "watchers <issue id>") (count: 1)))
   ("watcher-delete" (hash (description: "Get Watchers on issue") (usage: "watcher-delete <issue id> <username>") (count: 2)))
   ("watcher-add" (hash (description: "Get Watchers on issue") (usage: "watcher-delete <issue id> <username>") (count: 2)))
   ("label" (hash (description: "label a jira issue") (usage: "label <jira issue> <label>") (count: 2)))
   ("metadata" (hash (description: "Get definitions of fields available for issue.") (usage: "metadata <issue name>") (count: 1)))
   ("members" (hash (description: "Get list of members of a given project.") (usage: "members <Project Name>") (count: 1)))
   ("open" (hash (description: "Open Jira Issue in browser.") (usage: "open <Issue Number>") (count: 1)))
   ("projects" (hash (description: "List all projects") (usage: "projects") (count: 0)))
   ("properties" (hash (description: "Fetch all properties available for issue") (usage: "properties <issue id>") (count: 1)))
   ("property" (hash (description: "Fetch all properties available for issue") (usage: "properties <issue id>") (count: 1)))
   ("priorities" (hash (description: "List priorities available for issues") (usage: "priorities") (count: 0)))
   ("q" (hash (description: "Execute one of your stored queries in your ~/.jira.yaml" ) (usage: "q <query name>") (count: 1)))
   ("run" (hash (description: "Execute one of your stored creations in your ~/.jira.yaml" ) (usage: "run <creation name>") (count: 1)))
   ("search" (hash (description: "Search for issues matching string") (usage: "search <query string>") (count: 1)))
   ("transition" (hash (description: "Transition issue to new state.") (usage: "transition <issue name> <transition id>") (count: 2)))
   ("transition-comment" (hash (description: "Transition issue to new state while commenting.") (usage: "transition-comment <issue name> <transition id> <comment>") (count: 3)))
   ("transitions" (hash (description: "Get list of transitions available for issue") (usage: "transitions <issue name>") (count: 1)))))

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
      (apply (eval (string->symbol (string-append "ober/jira/client#" verb))) args2))))

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
