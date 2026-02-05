;;; -*- Gerbil -*-
;;; Jira CLI — Projects, Versions, Components, Categories, Roles

(import
  :ober/jira/api
  :ober/jira/format
  :std/format
  :std/iter
  :std/sugar
  :std/text/json)

(export #t)

;;;; Projects

(def (project-list config)
  "List all projects"
  (let ((items (jira-get-paginated config "/rest/api/3/project/search?"))
        (headers ["Id" "Key" "Name" "Type"])
        (rows []))
    (for (p items)
      (when (hash-table? p)
        (let-hash p
          (set! rows (cons [(or .?id "") (or .?key "") (or .?name "") (or .?projectTypeKey "")]
                           rows)))))
    (output headers (reverse rows))))

(def (project-get config key)
  "Get project details"
  (let ((body (jira-get config (format "/rest/api/3/project/~a" key))))
    (when (hash-table? body)
      (let-hash body
        (display-item "Name" .?name)
        (display-item "Key" .?key)
        (display-item "Id" .?id)
        (display-item "Type" .?projectTypeKey)
        (display-item "Description" .?description)
        (display-item "Lead" (if (hash-table? .?lead) (hash-get .lead 'displayName) ""))
        (display-item "URL" .?self)))))

(def (project-id-by-key config key)
  "Get project ID from key"
  (let ((body (jira-get config (format "/rest/api/3/project/~a" key))))
    (when (hash-table? body)
      (hash-get body 'id))))

;;;; Project Roles

(def (project-roles config key)
  "List project roles"
  (let ((body (jira-get config (format "/rest/api/3/project/~a/role" key)))
        (headers ["Role" "URL"])
        (rows []))
    (when (hash-table? body)
      (hash-for-each
       (lambda (k v)
         (set! rows (cons [(->string k) (->string v)] rows)))
       body))
    (output headers (reverse rows))))

(def (project-role-get config key role-id)
  "Get a specific project role"
  (let ((body (jira-get config (format "/rest/api/3/project/~a/role/~a" key role-id))))
    (when (hash-table? body)
      (let-hash body
        (display-item "Name" .?name)
        (display-item "Description" .?description)
        (when (and .?actors (list? .actors))
          (displayln "Actors:")
          (for (actor .actors)
            (when (hash-table? actor)
              (displayln (format "  ~a (~a)" (hash-get actor 'displayName) (hash-get actor 'type))))))))))

;;;; Versions

(def (project-versions config key)
  "List project versions"
  (let ((body (jira-get config (format "/rest/api/3/project/~a/versions" key)))
        (headers ["Id" "Name" "Description" "Released" "Archived" "Release Date"])
        (rows []))
    (when (list? body)
      (for (v body)
        (when (hash-table? v)
          (let-hash v
            (set! rows (cons [(or .?id "") (or .?name "") (or .?description "")
                              (yon .?released) (yon .?archived) (or .?releaseDate "")]
                             rows))))))
    (output headers (reverse rows))))

(def (version-create config project-key name description release-date)
  "Create a project version"
  (let ((project-id (project-id-by-key config project-key)))
    (let ((body (jira-post config "/rest/api/3/version"
                           (hash ("name" name)
                                 ("description" description)
                                 ("projectId" project-id)
                                 ("releaseDate" release-date)))))
      (if (hash-table? body)
        (let-hash body
          (displayln (format "Created version '~a' (id: ~a) in ~a" .?name .?id project-key)))
        (displayln (format "Created version '~a' in ~a" name project-key))))))

(def (version-get config version-id)
  "Get version details"
  (let ((body (jira-get config (format "/rest/api/3/version/~a" version-id))))
    (when (hash-table? body)
      (let-hash body
        (display-item "Id" .?id)
        (display-item "Name" .?name)
        (display-item "Description" .?description)
        (display-item "Released" (yon .?released))
        (display-item "Archived" (yon .?archived))
        (display-item "Release Date" .?releaseDate)
        (display-item "Start Date" .?startDate)))))

(def (version-update config version-id data)
  "Update a version"
  (jira-put config (format "/rest/api/3/version/~a" version-id) data)
  (displayln (format "Updated version ~a" version-id)))

(def (version-delete config version-id)
  "Delete a version"
  (jira-delete config (format "/rest/api/3/version/~a" version-id))
  (displayln (format "Deleted version ~a" version-id)))

;;;; Components

(def (project-components config key)
  "List project components"
  (let ((body (jira-get config (format "/rest/api/3/project/~a/components" key)))
        (headers ["Id" "Name" "Description" "Lead" "Assignee Type"])
        (rows []))
    (when (list? body)
      (for (c body)
        (when (hash-table? c)
          (let-hash c
            (set! rows (cons [(or .?id "") (or .?name "") (or .?description "")
                              (if (hash-table? .?lead) (hash-get .lead 'displayName) "")
                              (or .?assigneeType "")]
                             rows))))))
    (output headers (reverse rows))))

(def (component-create config project-key name description)
  "Create a project component"
  (let ((body (jira-post config "/rest/api/3/component"
                         (hash ("name" name)
                               ("description" description)
                               ("project" project-key)))))
    (if (hash-table? body)
      (let-hash body
        (displayln (format "Created component '~a' (id: ~a) in ~a" .?name .?id project-key)))
      (displayln (format "Created component '~a' in ~a" name project-key)))))

(def (component-get config component-id)
  "Get component details"
  (let ((body (jira-get config (format "/rest/api/3/component/~a" component-id))))
    (when (hash-table? body)
      (let-hash body
        (display-item "Id" .?id)
        (display-item "Name" .?name)
        (display-item "Description" .?description)
        (display-item "Lead" (if (hash-table? .?lead) (hash-get .lead 'displayName) ""))
        (display-item "Assignee Type" .?assigneeType)))))

(def (component-delete config component-id)
  "Delete a component"
  (jira-delete config (format "/rest/api/3/component/~a" component-id))
  (displayln (format "Deleted component ~a" component-id)))

;;;; Project Categories

(def (category-list config)
  "List project categories"
  (let ((body (jira-get config "/rest/api/3/projectCategory"))
        (headers ["Id" "Name" "Description"])
        (rows []))
    (when (list? body)
      (for (cat body)
        (when (hash-table? cat)
          (let-hash cat
            (set! rows (cons [(or .?id "") (or .?name "") (or .?description "")] rows))))))
    (output headers (reverse rows))))
