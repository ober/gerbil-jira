;;; -*- Gerbil -*-
;;; Jira CLI — Administration & Configuration

(import
  :ober/jira/api
  :ober/jira/format
  :std/format
  :std/iter
  :std/sugar
  :std/text/json)

(export #t)

;;;; Server Info

(def (server-info config)
  "Get Jira server information"
  (let ((body (jira-get config "/rest/api/3/serverInfo")))
    (when (hash-table? body)
      (let-hash body
        (display-item "Server Title" .?serverTitle)
        (display-item "Base URL" .?baseUrl)
        (display-item "Version" .?version)
        (display-item "Build Number" .?buildNumber)
        (display-item "Build Date" .?buildDate)
        (display-item "Server Time" .?serverTime)
        (display-item "Deploy Type" .?deploymentType)
        (display-item "SCM Info" .?scmInfo)))))

;;;; Statuses

(def (status-list config)
  "List all issue statuses"
  (let ((body (jira-get config "/rest/api/3/status"))
        (headers ["Id" "Name" "Description" "Category"])
        (rows []))
    (when (list? body)
      (for (s body)
        (when (hash-table? s)
          (let-hash s
            (set! rows (cons [(or .?id "") (or .?name "") (or .?description "")
                              (if (hash-table? .?statusCategory)
                                (hash-get .statusCategory 'name)
                                "")]
                             rows))))))
    (output headers (reverse rows))))

;;;; Priorities

(def (priority-list config)
  "List issue priorities"
  (let ((body (jira-get config "/rest/api/3/priority"))
        (headers ["Id" "Name" "Description" "Status Color" "Icon URL"])
        (rows []))
    (when (list? body)
      (for (p body)
        (when (hash-table? p)
          (let-hash p
            (set! rows (cons [(or .?id "") (or .?name "") (or .?description "")
                              (or .?statusColor "") (or .?iconUrl "")]
                             rows))))))
    (output headers (reverse rows))))

;;;; Resolutions

(def (resolution-list config)
  "List issue resolutions"
  (let ((body (jira-get config "/rest/api/3/resolution"))
        (headers ["Id" "Name" "Description"])
        (rows []))
    (when (list? body)
      (for (r body)
        (when (hash-table? r)
          (let-hash r
            (set! rows (cons [(or .?id "") (or .?name "") (or .?description "")] rows))))))
    (output headers (reverse rows))))

;;;; Issue Types

(def (issuetype-list config)
  "List issue types"
  (let ((body (jira-get config "/rest/api/3/issuetype"))
        (headers ["Id" "Name" "Description" "Subtask" "Icon URL"])
        (rows []))
    (when (list? body)
      (for (it body)
        (when (hash-table? it)
          (let-hash it
            (set! rows (cons [(or .?id "") (or .?name "") (or .?description "")
                              (yon .?subtask) (or .?iconUrl "")]
                             rows))))))
    (output headers (reverse rows))))

(def (issuetype-get config type-id)
  "Get issue type details"
  (let ((body (jira-get config (format "/rest/api/3/issuetype/~a" type-id))))
    (when (hash-table? body)
      (let-hash body
        (display-item "Id" .?id)
        (display-item "Name" .?name)
        (display-item "Description" .?description)
        (display-item "Subtask" (yon .?subtask))
        (display-item "Icon URL" .?iconUrl)))))

;;;; Fields

(def (field-list config)
  "List all fields"
  (let ((body (jira-get config "/rest/api/3/field"))
        (headers ["Id" "Name" "Key" "Custom" "Navigable" "Searchable" "Orderable"])
        (rows []))
    (when (list? body)
      (for (f body)
        (when (hash-table? f)
          (let-hash f
            (set! rows (cons [(or .?id "") (or .?name "") (or .?key "")
                              (yon .?custom) (yon .?navigable)
                              (yon .?searchable) (yon .?orderable)]
                             rows))))))
    (output headers (reverse rows))))

;;;; Filters

(def (filter-list config)
  "List user's filters"
  (let ((body (jira-get config "/rest/api/3/filter/my"))
        (headers ["Id" "Name" "Owner" "JQL"])
        (rows []))
    (when (list? body)
      (for (f body)
        (when (hash-table? f)
          (let-hash f
            (set! rows (cons [(or .?id "") (or .?name "")
                              (if (hash-table? .?owner) (hash-get .owner 'displayName) "")
                              (or .?jql "")]
                             rows))))))
    (output headers (reverse rows))))

(def (filter-get config filter-id)
  "Get filter details"
  (let ((body (jira-get config (format "/rest/api/3/filter/~a" filter-id))))
    (when (hash-table? body)
      (let-hash body
        (display-item "Id" .?id)
        (display-item "Name" .?name)
        (display-item "Owner" (if (hash-table? .?owner) (hash-get .owner 'displayName) ""))
        (display-item "JQL" .?jql)
        (display-item "Favourite" (yon .?favourite))))))

(def (filter-search config (query ""))
  "Search filters"
  (let ((body (jira-get config (format "/rest/api/3/filter/search?filterName=~a" query)))
        (headers ["Id" "Name" "Owner" "JQL"])
        (rows []))
    (when (hash-table? body)
      (let-hash body
        (when (and .?values (list? .values))
          (for (f .values)
            (when (hash-table? f)
              (let-hash f
                (set! rows (cons [(or .?id "") (or .?name "")
                                  (if (hash-table? .?owner) (hash-get .owner 'displayName) "")
                                  (or .?jql "")]
                                 rows))))))))
    (output headers (reverse rows))))

;;;; Dashboards

(def (dashboard-list config)
  "List dashboards"
  (let ((body (jira-get config "/rest/api/3/dashboard"))
        (headers ["Id" "Name" "Owner" "View"])
        (rows []))
    (when (hash-table? body)
      (let-hash body
        (when (and .?dashboards (list? .dashboards))
          (for (d .dashboards)
            (when (hash-table? d)
              (let-hash d
                (set! rows (cons [(or .?id "") (or .?name "")
                                  (if (hash-table? .?owner) (hash-get .owner 'displayName) "")
                                  (or .?view "")]
                                 rows))))))))
    (output headers (reverse rows))))

(def (dashboard-get config dashboard-id)
  "Get dashboard details"
  (let ((body (jira-get config (format "/rest/api/3/dashboard/~a" dashboard-id))))
    (when (hash-table? body)
      (let-hash body
        (display-item "Id" .?id)
        (display-item "Name" .?name)
        (display-item "Owner" (if (hash-table? .?owner) (hash-get .owner 'displayName) ""))
        (display-item "View" .?view)
        (display-item "Favourite" (yon .?isFavourite))))))

;;;; Create Metadata

(def (create-meta config project)
  "Get create metadata for a project"
  (let ((body (jira-get config (format "/rest/api/3/issue/createmeta?projectKeys=~a" project)))
        (headers ["Id" "Name" "Description" "Subtask" "Icon URL"])
        (rows []))
    (when (hash-table? body)
      (let-hash body
        (when (and .?projects (list? .projects))
          (let-hash (car .projects)
            (when .?issuetypes
              (for (it .issuetypes)
                (when (hash-table? it)
                  (let-hash it
                    (set! rows (cons [(or .?id "") (or .?name "") (or .?description "")
                                      (yon .?subtask) (or .?iconUrl "")]
                                     rows))))))))))
    (output headers (reverse rows))))

;;;; Labels

(def (label-list config)
  "List all labels"
  (let ((body (jira-get config "/rest/api/3/label"))
        (headers ["Label"])
        (rows []))
    (when (hash-table? body)
      (let-hash body
        (when (and .?values (list? .values))
          (for (label .values)
            (set! rows (cons [(->string label)] rows))))))
    (output headers (reverse rows))))

;;;; Workflows

(def (workflow-list config)
  "List workflows"
  (let ((body (jira-get config "/rest/api/3/workflow/search"))
        (headers ["Name" "Description" "Steps" "Default"])
        (rows []))
    (when (hash-table? body)
      (let-hash body
        (when (and .?values (list? .values))
          (for (wf .values)
            (when (hash-table? wf)
              (let-hash wf
                (set! rows (cons [(or .?name "") (or .?description "")
                                  (if (list? .?statuses) (length .statuses) 0)
                                  (yon .?isDefault)]
                                 rows))))))))
    (output headers (reverse rows))))

;;;; Audit records

(def (audit-list config)
  "List audit records"
  (let ((body (jira-get config "/rest/api/3/auditing/record"))
        (headers ["Id" "Summary" "Category" "Created" "Author"])
        (rows []))
    (when (hash-table? body)
      (let-hash body
        (when (and .?records (list? .records))
          (for (r .records)
            (when (hash-table? r)
              (let-hash r
                (set! rows (cons [(or .?id "") (or .?summary "") (or .?category "")
                                  (date-short .?created)
                                  (if (hash-table? .?authorKey) (hash-get .authorKey 'displayName) "")]
                                 rows))))))))
    (output headers (reverse rows))))

;;;; Notification Schemes

(def (notification-scheme-list config)
  "List notification schemes"
  (let ((body (jira-get config "/rest/api/3/notificationscheme"))
        (headers ["Id" "Name" "Description"])
        (rows []))
    (when (hash-table? body)
      (let-hash body
        (when (and .?values (list? .values))
          (for (ns .values)
            (when (hash-table? ns)
              (let-hash ns
                (set! rows (cons [(or .?id "") (or .?name "") (or .?description "")] rows))))))))
    (output headers (reverse rows))))

;;;; Permission Schemes

(def (permission-scheme-list config)
  "List permission schemes"
  (let ((body (jira-get config "/rest/api/3/permissionscheme"))
        (headers ["Id" "Name" "Description"])
        (rows []))
    (when (hash-table? body)
      (let-hash body
        (when (and .?permissionSchemes (list? .permissionSchemes))
          (for (ps .permissionSchemes)
            (when (hash-table? ps)
              (let-hash ps
                (set! rows (cons [(or .?id "") (or .?name "") (or .?description "")] rows))))))))
    (output headers (reverse rows))))

;;;; Security Schemes

(def (security-scheme-list config)
  "List issue security schemes"
  (let ((body (jira-get config "/rest/api/3/issuesecurityschemes"))
        (headers ["Id" "Name" "Description"])
        (rows []))
    (when (hash-table? body)
      (let-hash body
        (when (and .?issueSecuritySchemes (list? .issueSecuritySchemes))
          (for (ss .issueSecuritySchemes)
            (when (hash-table? ss)
              (let-hash ss
                (set! rows (cons [(or .?id "") (or .?name "") (or .?description "")] rows))))))))
    (output headers (reverse rows))))

;;;; Screens

(def (screen-list config)
  "List screens"
  (let ((body (jira-get config "/rest/api/3/screens"))
        (headers ["Id" "Name" "Description"])
        (rows []))
    (when (hash-table? body)
      (let-hash body
        (when (and .?values (list? .values))
          (for (s .values)
            (when (hash-table? s)
              (let-hash s
                (set! rows (cons [(or .?id "") (or .?name "") (or .?description "")] rows))))))))
    (output headers (reverse rows))))

;;;; Configuration

(def (server-config config)
  "Get server configuration"
  (let ((body (jira-get config "/rest/api/3/configuration")))
    (output-json body)))
