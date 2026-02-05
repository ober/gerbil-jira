;;; -*- Gerbil -*-
;;; Jira CLI — Entry Point

(import
  :jira/jira/admin
  :jira/jira/agile
  :jira/jira/api
  :jira/jira/config
  :jira/jira/format
  :jira/jira/issues
  :jira/jira/links
  :jira/jira/projects
  :jira/jira/properties
  :jira/jira/search
  :jira/jira/users
  :std/format
  :std/getopt
  :std/pregexp
  :std/sugar)

(export main)

(def (main . args)
  (call-with-getopt jira-main args
    program: "jira"
    help: (format "Jira CLI v~a" version)

    ;; Global flags
    (flag 'json "--json" help: "Output as JSON")
    (flag 'csv "--csv" help: "Output as CSV")

    ;; Issue commands
    (command 'issue help: "Get issue details"
      (argument 'id help: "Issue key (e.g. PROJ-123)"))
    (command 'create help: "Create a new issue"
      (argument 'project help: "Project key")
      (argument 'summary help: "Issue summary")
      (argument 'description help: "Issue description"))
    (command 'delete-issue help: "Delete an issue"
      (argument 'id help: "Issue key"))
    (command 'assign help: "Assign issue to user"
      (argument 'id help: "Issue key")
      (argument 'user help: "Username"))
    (command 'label help: "Add label to issue"
      (argument 'id help: "Issue key")
      (argument 'label help: "Label name"))
    (command 'update-field help: "Update a field on an issue"
      (argument 'id help: "Issue key")
      (argument 'field help: "Field name")
      (argument 'value help: "Field value"))
    (command 'changelog help: "Get issue changelog"
      (argument 'id help: "Issue key"))
    (command 'open help: "Open issue in browser"
      (argument 'id help: "Issue key"))

    ;; Transition commands
    (command 'transitions help: "List available transitions"
      (argument 'id help: "Issue key"))
    (command 'transition help: "Transition issue to new state"
      (argument 'id help: "Issue key")
      (argument 'transition-id help: "Transition ID"))
    (command 'transition-comment help: "Transition issue with comment"
      (argument 'id help: "Issue key")
      (argument 'transition-id help: "Transition ID")
      (argument 'comment help: "Comment text"))

    ;; Comment commands
    (command 'comment help: "Add comment to issue"
      (argument 'id help: "Issue key")
      (argument 'text help: "Comment text"))
    (command 'comments help: "List comments on issue"
      (argument 'id help: "Issue key"))
    (command 'comment-update help: "Update a comment"
      (argument 'id help: "Issue key")
      (argument 'comment-id help: "Comment ID")
      (argument 'text help: "New text"))
    (command 'comment-delete help: "Delete a comment"
      (argument 'id help: "Issue key")
      (argument 'comment-id help: "Comment ID"))

    ;; Vote commands
    (command 'vote help: "Vote on an issue"
      (argument 'id help: "Issue key"))
    (command 'unvote help: "Remove vote from issue"
      (argument 'id help: "Issue key"))
    (command 'votes help: "Get vote info for issue"
      (argument 'id help: "Issue key"))

    ;; Watcher commands
    (command 'watchers help: "List watchers on issue"
      (argument 'id help: "Issue key"))
    (command 'watcher-add help: "Add watcher to issue"
      (argument 'id help: "Issue key")
      (argument 'user help: "Username"))
    (command 'watcher-del help: "Remove watcher from issue"
      (argument 'id help: "Issue key")
      (argument 'user help: "Username"))

    ;; Worklog commands
    (command 'work help: "List worklogs for issue"
      (argument 'id help: "Issue key"))
    (command 'worklog-add help: "Add worklog entry"
      (argument 'id help: "Issue key")
      (argument 'time help: "Time spent (e.g. 2h, 30m)")
      (optional-argument 'comment help: "Comment"))

    ;; Attachment commands
    (command 'attachment help: "Get attachment details"
      (argument 'id help: "Attachment ID"))
    (command 'attachment-meta help: "Get attachment upload limits")
    (command 'attachment-delete help: "Delete an attachment"
      (argument 'id help: "Attachment ID"))

    ;; Link commands
    (command 'link-create help: "Create issue link"
      (argument 'type help: "Link type name")
      (argument 'inward help: "Inward issue key")
      (argument 'outward help: "Outward issue key"))
    (command 'link-get help: "Get link details"
      (argument 'id help: "Link ID"))
    (command 'link-delete help: "Delete issue link"
      (argument 'id help: "Link ID"))
    (command 'link-types help: "List issue link types")

    ;; Remote link commands
    (command 'remote-links help: "List remote links on issue"
      (argument 'id help: "Issue key"))
    (command 'remote-link-create help: "Create remote link"
      (argument 'id help: "Issue key")
      (argument 'url help: "Link URL")
      (argument 'title help: "Link title"))
    (command 'remote-link-delete help: "Delete remote link"
      (argument 'id help: "Issue key")
      (argument 'link-id help: "Remote link ID"))

    ;; Property commands
    (command 'properties help: "List issue properties"
      (argument 'id help: "Issue key"))
    (command 'property-get help: "Get issue property"
      (argument 'id help: "Issue key")
      (argument 'key help: "Property key"))
    (command 'property-set help: "Set issue property"
      (argument 'id help: "Issue key")
      (argument 'key help: "Property key")
      (argument 'value help: "Property value"))
    (command 'property-delete help: "Delete issue property"
      (argument 'id help: "Issue key")
      (argument 'key help: "Property key"))

    ;; Search commands
    (command 'search help: "Search for issues"
      (argument 'query help: "JQL query or text"))
    (command 'q help: "Run saved query from config"
      (argument 'alias help: "Query alias name"))
    (command 'run help: "Run saved creation template"
      (argument 'name help: "Creation template name"))

    ;; Project commands
    (command 'projects help: "List all projects")
    (command 'get-project help: "Get project details"
      (argument 'key help: "Project key"))
    (command 'project-roles help: "List project roles"
      (argument 'key help: "Project key"))
    (command 'project-versions help: "List project versions"
      (argument 'key help: "Project key"))
    (command 'components help: "List project components"
      (argument 'key help: "Project key"))
    (command 'component-create help: "Create project component"
      (argument 'key help: "Project key")
      (argument 'name help: "Component name")
      (argument 'description help: "Component description"))
    (command 'version-create help: "Create project version"
      (argument 'key help: "Project key")
      (argument 'name help: "Version name")
      (argument 'description help: "Version description")
      (argument 'release-date help: "Release date (YYYY-MM-DD)"))
    (command 'categories help: "List project categories")

    ;; User commands
    (command 'myself help: "Get current user details")
    (command 'users help: "List all users")
    (command 'user-search help: "Search for users"
      (argument 'query help: "Search query"))
    (command 'members help: "List group members"
      (argument 'group help: "Group name"))
    (command 'groups help: "List groups"
      (optional-argument 'query help: "Search query"))
    (command 'permissions help: "List my permissions")

    ;; Agile commands
    (command 'boards help: "List agile boards")
    (command 'board-get help: "Get board details"
      (argument 'id help: "Board ID"))
    (command 'sprints help: "List sprints for a board"
      (argument 'board-id help: "Board ID"))
    (command 'sprint-get help: "Get sprint details"
      (argument 'id help: "Sprint ID"))
    (command 'sprint-issues help: "List issues in a sprint"
      (argument 'id help: "Sprint ID"))
    (command 'sprint-create help: "Create a sprint"
      (argument 'board-id help: "Board ID")
      (argument 'name help: "Sprint name")
      (optional-argument 'goal help: "Sprint goal"))
    (command 'epics help: "List epics for a board"
      (argument 'board-id help: "Board ID"))
    (command 'epic-issues help: "List issues in an epic"
      (argument 'id help: "Epic ID"))
    (command 'backlog help: "List backlog issues"
      (argument 'board-id help: "Board ID"))

    ;; Admin commands
    (command 'server-info help: "Get server information")
    (command 'statuses help: "List issue statuses")
    (command 'priorities help: "List issue priorities")
    (command 'resolutions help: "List issue resolutions")
    (command 'fields help: "List all fields")
    (command 'filters help: "List my filters")
    (command 'filter-get help: "Get filter details"
      (argument 'id help: "Filter ID"))
    (command 'dashboards help: "List dashboards")
    (command 'dashboard-get help: "Get dashboard details"
      (argument 'id help: "Dashboard ID"))
    (command 'metas help: "Get create metadata"
      (argument 'project help: "Project key"))
    (command 'issuetypes help: "List issue types")
    (command 'issuetype-get help: "Get issue type details"
      (argument 'id help: "Issue type ID"))
    (command 'labels help: "List all labels")
    (command 'workflows help: "List workflows")
    (command 'screens help: "List screens")
    (command 'notification-schemes help: "List notification schemes")
    (command 'permission-schemes help: "List permission schemes")
    (command 'security-schemes help: "List issue security schemes")
    (command 'server-config help: "Get server configuration")
    (command 'audit help: "List audit records")

    ;; Config command
    (command 'config help: "Setup credentials")))

(def (jira-main cmd opt)
  ;; Set output format from global flags
  (when (hash-get opt 'json)
    (current-output-format 'json))
  (when (hash-get opt 'csv)
    (current-output-format 'csv))

  (case cmd
    ;; Config (no auth needed)
    ((config) (config-setup!))

    ;; Open in browser (needs config but simple)
    ((open)
     (let ((config (load-config)))
       (issue-open config (hash-get opt 'id))))

    ;; All other commands need config
    (else
     (let ((config (load-config)))
       (case cmd
           ;; Issues
           ((issue) (issue-get config (hash-get opt 'id)))
           ((create) (issue-create config (hash-get opt 'project)
                                   (hash-get opt 'summary) (hash-get opt 'description)))
           ((delete-issue) (issue-delete config (hash-get opt 'id)))
           ((assign) (issue-assign config (hash-get opt 'id) (hash-get opt 'user)))
           ((label) (issue-label config (hash-get opt 'id) (hash-get opt 'label)))
           ((update-field) (issue-update-field config (hash-get opt 'id)
                                               (hash-get opt 'field) (hash-get opt 'value)))
           ((changelog) (issue-changelog config (hash-get opt 'id)))

           ;; Transitions
           ((transitions) (issue-transitions config (hash-get opt 'id)))
           ((transition) (issue-transition config (hash-get opt 'id)
                                           (hash-get opt 'transition-id)))
           ((transition-comment) (issue-transition-comment config (hash-get opt 'id)
                                                           (hash-get opt 'transition-id)
                                                           (hash-get opt 'comment)))

           ;; Comments
           ((comment) (comment-add config (hash-get opt 'id) (hash-get opt 'text)))
           ((comments) (comment-list config (hash-get opt 'id)))
           ((comment-update) (comment-update config (hash-get opt 'id)
                                             (hash-get opt 'comment-id)
                                             (hash-get opt 'text)))
           ((comment-delete) (comment-delete config (hash-get opt 'id)
                                             (hash-get opt 'comment-id)))

           ;; Votes
           ((vote) (vote-add config (hash-get opt 'id)))
           ((unvote) (vote-remove config (hash-get opt 'id)))
           ((votes) (vote-get config (hash-get opt 'id)))

           ;; Watchers
           ((watchers) (watcher-list config (hash-get opt 'id)))
           ((watcher-add) (watcher-add config (hash-get opt 'id) (hash-get opt 'user)))
           ((watcher-del) (watcher-remove config (hash-get opt 'id) (hash-get opt 'user)))

           ;; Worklogs
           ((work) (worklog-list config (hash-get opt 'id)))
           ((worklog-add) (worklog-add config (hash-get opt 'id)
                                       (hash-get opt 'time)
                                       (hash-get opt 'comment)))

           ;; Attachments
           ((attachment) (attachment-get config (hash-get opt 'id)))
           ((attachment-meta) (attachment-meta config))
           ((attachment-delete) (attachment-delete config (hash-get opt 'id)))

           ;; Links
           ((link-create) (link-create config (hash-get opt 'type)
                                       (hash-get opt 'inward) (hash-get opt 'outward)))
           ((link-get) (link-get config (hash-get opt 'id)))
           ((link-delete) (link-delete config (hash-get opt 'id)))
           ((link-types) (link-types config))

           ;; Remote links
           ((remote-links) (remote-link-list config (hash-get opt 'id)))
           ((remote-link-create) (remote-link-create config (hash-get opt 'id)
                                                     (hash-get opt 'url)
                                                     (hash-get opt 'title)))
           ((remote-link-delete) (remote-link-delete config (hash-get opt 'id)
                                                     (hash-get opt 'link-id)))

           ;; Properties
           ((properties) (property-list config (hash-get opt 'id)))
           ((property-get) (property-get config (hash-get opt 'id) (hash-get opt 'key)))
           ((property-set) (property-set config (hash-get opt 'id)
                                         (hash-get opt 'key) (hash-get opt 'value)))
           ((property-delete) (property-delete config (hash-get opt 'id) (hash-get opt 'key)))

           ;; Search
           ((search) (search-issues config (hash-get opt 'query)))
           ((q) (run-query config (hash-get opt 'alias)))
           ((run) (run-creation config (hash-get opt 'name)))

           ;; Projects
           ((projects) (project-list config))
           ((get-project) (project-get config (hash-get opt 'key)))
           ((project-roles) (project-roles config (hash-get opt 'key)))
           ((project-versions) (project-versions config (hash-get opt 'key)))
           ((components) (project-components config (hash-get opt 'key)))
           ((component-create) (component-create config (hash-get opt 'key)
                                                 (hash-get opt 'name)
                                                 (hash-get opt 'description)))
           ((version-create) (version-create config (hash-get opt 'key)
                                             (hash-get opt 'name)
                                             (hash-get opt 'description)
                                             (hash-get opt 'release-date)))
           ((categories) (category-list config))

           ;; Users
           ((myself) (myself config))
           ((users) (user-list config))
           ((user-search) (user-search config (hash-get opt 'query)))
           ((members) (group-members config (hash-get opt 'group)))
           ((groups) (group-list config (or (hash-get opt 'query) "")))
           ((permissions) (my-permissions config))

           ;; Agile
           ((boards) (board-list config))
           ((board-get) (board-get config (hash-get opt 'id)))
           ((sprints) (sprint-list config (hash-get opt 'board-id)))
           ((sprint-get) (sprint-get config (hash-get opt 'id)))
           ((sprint-issues) (sprint-issues config (hash-get opt 'id)))
           ((sprint-create) (sprint-create config (hash-get opt 'board-id)
                                           (hash-get opt 'name)
                                           (or (hash-get opt 'goal) "")))
           ((epics) (epic-list config (hash-get opt 'board-id)))
           ((epic-issues) (epic-issues config (hash-get opt 'id)))
           ((backlog) (board-backlog config (hash-get opt 'board-id)))

           ;; Admin
           ((server-info) (server-info config))
           ((statuses) (status-list config))
           ((priorities) (priority-list config))
           ((resolutions) (resolution-list config))
           ((fields) (field-list config))
           ((filters) (filter-list config))
           ((filter-get) (filter-get config (hash-get opt 'id)))
           ((dashboards) (dashboard-list config))
           ((dashboard-get) (dashboard-get config (hash-get opt 'id)))
           ((metas) (create-meta config (hash-get opt 'project)))
           ((issuetypes) (issuetype-list config))
           ((issuetype-get) (issuetype-get config (hash-get opt 'id)))
           ((labels) (label-list config))
           ((workflows) (workflow-list config))
           ((screens) (screen-list config))
           ((notification-schemes) (notification-scheme-list config))
           ((permission-schemes) (permission-scheme-list config))
           ((security-schemes) (security-scheme-list config))
           ((server-config) (server-config config))
           ((audit) (audit-list config))

           (else
            (displayln (format "Unknown command: ~a" cmd))
            (exit 2)))))))

(def (issue-open config id)
  "Open issue in browser"
  (unless (pregexp-match "^[A-Za-z0-9_-]+$" id)
    (error "Invalid issue ID format"))
  (let-hash config
    (let* ((command (cond-expand
                      (darwin "open")
                      (linux "xdg-open")
                      (else "xdg-open")))
           (url (format "~a/browse/~a" .url id)))
      (let ((proc (open-process [path: command arguments: [url]])))
        (process-status proc)
        (close-port proc)))))
