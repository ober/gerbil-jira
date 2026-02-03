;;; -*- Gerbil -*-
;;; © ober
;;; Jira client binary

(import
  :clan/text/yaml
  :gerbil/gambit
  :ober/jira/client
  :ober/oberlib
  :std/db/dbi
  :std/debug/heap
  :std/error
  :std/format
  :std/generic
  :std/generic/dispatch
  :std/iter
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
  :std/text/zlib
  )

(export main)

;;(declare (not optimize-dead-definitions))

(def program-name "jira")

;;; CLI Command Dispatch Table
;;; Maps command names to their corresponding function references
;;; Used instead of eval for security
(def cli-commands
  (hash
   ("assign" assign)
   ("boards" boards)
   ("changelog" changelog)
   ("comment" comment)
   ("comment-delete" delete-comment)
   ("comment-update" update-comment)
   ("component-create" create-component)
   ("components" project-components)
   ("config" config)
   ("create" create)
   ("dashboards" dashboards)
   ("delete-issue" delete-issue)
   ("fields" fields)
   ("filters" filters)
   ("get-project" get-project)
   ("groups" groups)
   ("issue" issue)
   ("label" label)
   ("link-create" create-link)
   ("link-delete" delete-link)
   ("link-get" get-link)
   ("link-types" link-types)
   ("members" members)
   ("metas" metas)
   ("myself" myself)
   ("open" open)
   ("priorities" priorities)
   ("project-roles" project-roles)
   ("project-versions" project-versions)
   ("projects" projects)
   ("properties" properties)
   ("q" q)
   ("remote-link-create" create-remote-link)
   ("remote-link-delete" delete-remote-link)
   ("remote-links" remote-links)
   ("resolutions" resolutions)
   ("run" run)
   ("search" search)
   ("server-info" server-info)
   ("sprint-issues" sprint-issues)
   ("sprints" sprints)
   ("statuses" statuses)
   ("transition" transition)
   ("transition-comment" transition-comment)
   ("transitions" transitions)
   ("update-field" update-field)
   ("users" users)
   ("version-create" create-version)
   ("work" work)
   ("watcher-add" watcher-add)
   ("watcher-del" watcher-del)
   ("watchers" watchers)))

(def interactives
  (hash
   ("assign" (hash (description: "Assign Issue to user") (usage: "assign <issue id> <user>") (count: 2)))
   ("boards" (hash (description: "List agile boards") (usage: "boards") (count: 0)))
   ("changelog" (hash (description: "Get issue changelog") (usage: "changelog <issue id>") (count: 1)))
   ("comment" (hash (description: "Add comment to Jira issue") (usage: "comment <issue> <comment>") (count: 2)))
   ("comment-delete" (hash (description: "Delete comment from issue") (usage: "comment-delete <issue id> <comment id>") (count: 2)))
   ("comment-update" (hash (description: "Update comment on issue") (usage: "comment-update <issue id> <comment id> <comment text>") (count: 3)))
   ("component-create" (hash (description: "Create project component") (usage: "component-create <project key> <name> <description>") (count: 3)))
   ("components" (hash (description: "Get project components") (usage: "components <project key>") (count: 1)))
   ("config" (hash (description: "Setup your user and password in the config encrypted") (usage: "config") (count: 0)))
   ("create" (hash (description: "Create new jira issue") (usage: "create <project> <summary> <description>") (count: 3)))
   ("dashboards" (hash (description: "List dashboards") (usage: "dashboards") (count: 0)))
   ("delete-issue" (hash (description: "Delete an issue") (usage: "delete-issue <issue id>") (count: 1)))
   ("fields" (hash (description: "Return all fields") (usage: "fields") (count: 0)))
   ("filters" (hash (description: "Get all search filters") (usage: "filters") (count: 0)))
   ("get-project" (hash (description: "Get project details") (usage: "get-project <project key>") (count: 1)))
   ("groups" (hash (description: "Search groups") (usage: "groups <query>") (count: 1)))
   ("issue" (hash (description: "Get Jira issue details") (usage: "issue <issue id>") (count: 1)))
   ("label" (hash (description: "Label a jira issue") (usage: "label <jira issue> <label>") (count: 2)))
   ("link-create" (hash (description: "Create issue link") (usage: "link-create <link type> <inward issue> <outward issue>") (count: 3)))
   ("link-delete" (hash (description: "Delete issue link") (usage: "link-delete <link id>") (count: 1)))
   ("link-get" (hash (description: "Get issue link details") (usage: "link-get <link id>") (count: 1)))
   ("link-types" (hash (description: "Get issue link types") (usage: "link-types") (count: 0)))
   ("members" (hash (description: "Get list of members of a given group") (usage: "members <group name>") (count: 1)))
   ("metas" (hash (description: "Print out server side configurations for your account") (usage: "metas") (count: 0)))
   ("myself" (hash (description: "Get current user details") (usage: "myself") (count: 0)))
   ("open" (hash (description: "Open Jira Issue in browser") (usage: "open <issue id>") (count: 1)))
   ("priorities" (hash (description: "List priorities available for issues") (usage: "priorities") (count: 0)))
   ("project-roles" (hash (description: "Get project roles") (usage: "project-roles <project key>") (count: 1)))
   ("project-versions" (hash (description: "Get project versions") (usage: "project-versions <project key>") (count: 1)))
   ("projects" (hash (description: "List all projects") (usage: "projects") (count: 0)))
   ("properties" (hash (description: "Fetch all properties available for issue") (usage: "properties <issue id>") (count: 1)))
   ("q" (hash (description: "Execute one of your stored queries in your ~/.jira.yaml") (usage: "q <query name>") (count: 1)))
   ("remote-link-create" (hash (description: "Create remote link on issue") (usage: "remote-link-create <issue id> <url> <title>") (count: 3)))
   ("remote-link-delete" (hash (description: "Delete remote link from issue") (usage: "remote-link-delete <issue id> <link id>") (count: 2)))
   ("remote-links" (hash (description: "Get remote links for issue") (usage: "remote-links <issue id>") (count: 1)))
   ("resolutions" (hash (description: "List issue resolutions") (usage: "resolutions") (count: 0)))
   ("run" (hash (description: "Execute one of your stored creations in your ~/.jira.yaml") (usage: "run <creation name>") (count: 1)))
   ("search" (hash (description: "Search for issues matching string") (usage: "search <query string>") (count: 1)))
   ("server-info" (hash (description: "Get Jira server information") (usage: "server-info") (count: 0)))
   ("sprint-issues" (hash (description: "List issues in a sprint") (usage: "sprint-issues <sprint id>") (count: 1)))
   ("sprints" (hash (description: "List sprints for a board") (usage: "sprints <board id>") (count: 1)))
   ("statuses" (hash (description: "List all issue statuses") (usage: "statuses") (count: 0)))
   ("transition" (hash (description: "Transition issue to new state") (usage: "transition <issue name> <transition id>") (count: 2)))
   ("transition-comment" (hash (description: "Transition issue with comment") (usage: "transition-comment <issue name> <transition id> <comment>") (count: 3)))
   ("transitions" (hash (description: "Get list of transitions available for issue") (usage: "transitions <issue name>") (count: 1)))
   ("update-field" (hash (description: "Update a custom field with the new value") (usage: "update-field <jira issue> <fieldname> <content>") (count: 3)))
   ("users" (hash (description: "Get list of users") (usage: "users") (count: 0)))
   ("version-create" (hash (description: "Create project version") (usage: "version-create <project key> <name> <description> <release date>") (count: 4)))
   ("work" (hash (description: "Get work log for issue") (usage: "work <issue id>") (count: 1)))
   ("watcher-add" (hash (description: "Add watcher to issue") (usage: "watcher-add <issue id> <username>") (count: 2)))
   ("watcher-del" (hash (description: "Remove watcher from issue") (usage: "watcher-del <issue id> <username>") (count: 2)))
   ("watchers" (hash (description: "Get watchers on issue") (usage: "watchers <issue id>") (count: 1)))
   ))

(def (main . args)
  (when (null? args)
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
      ;; Use dispatch table instead of eval for security
      (let ((handler (hash-get cli-commands verb)))
        (if handler
          (apply handler args2)
          (begin
            (displayln "Error: Unknown command: " verb)
            (exit 2)))))))

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
