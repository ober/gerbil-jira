;;; -*- Gerbil -*-
;;; Jira CLI — Users, Groups, Permissions

(import
  :jira/jira/api
  :jira/jira/format
  :std/format
  :std/iter
  :std/pregexp
  :std/sugar
  :std/text/json)

(export #t)

;;;; User identity caches (populated lazily)
(def user-to-id #f)
(def id-to-user #f)

;;;; Current user

(def (myself config)
  "Get current user details"
  (let ((body (jira-get config "/rest/api/3/myself")))
    (when (hash-table? body)
      (let-hash body
        (display-item "Display Name" .?displayName)
        (display-item "Email" .?emailAddress)
        (display-item "Account ID" .?accountId)
        (display-item "Active" (yon .?active))
        (display-item "Timezone" .?timeZone)
        (display-item "Account Type" .?accountType)
        (display-item "Locale" .?locale)))))

;;;; User search and listing

(def (user-search config query)
  "Search for users"
  (let ((body (jira-get config (format "/rest/api/3/user/search?query=~a" query)))
        (headers ["Name" "Email" "Display Name" "Active" "Account Type"])
        (rows []))
    (when (list? body)
      (for (u body)
        (when (hash-table? u)
          (let-hash u
            (set! rows (cons [(or .?name "")
                              (or .?emailAddress "")
                              (or .?displayName "")
                              (yon .?active)
                              (or .?accountType "")]
                             rows))))))
    (output headers (reverse rows))))

(def (user-list config)
  "List all users (paginated)"
  (let ((headers ["Display Name" "Email" "Account ID" "Active" "Account Type"])
        (rows []))
    (let loop ((offset 0))
      (let ((body (jira-get config (format "/rest/api/3/users/search?startAt=~a&maxResults=200" offset))))
        (when (and (list? body) (pair? body))
          (for (u body)
            (when (hash-table? u)
              (let-hash u
                (set! rows (cons [(or .?displayName "")
                                  (or .?emailAddress "")
                                  (or .?accountId "")
                                  (yon .?active)
                                  (or .?accountType "")]
                                 rows)))))
          (when (>= (length body) 200)
            (loop (+ offset 200))))))
    (output headers (reverse rows))))

;;;; User ID mapping

(def (fetch-all-users config)
  "Fetch all users from API"
  (let ((users []))
    (let loop ((offset 0))
      (let ((body (jira-get config (format "/rest/api/3/users/search?startAt=~a&maxResults=1000" offset))))
        (when (and (list? body) (pair? body))
          (for (u body)
            (when (hash-table? u)
              (set! users (cons u users))))
          (when (>= (length body) 1000)
            (loop (+ offset 1000))))))
    users))

(def (make-user-hashes config)
  "Build user-to-id and id-to-user mappings"
  (unless user-to-id
    (set! user-to-id (hash))
    (set! id-to-user (hash))
    (let ((users (fetch-all-users config)))
      (for (u users)
        (when (hash-table? u)
          (let-hash u
            (let ((short (email-short .?emailAddress)))
              (when short
                (hash-put! user-to-id short .?accountId))
              (when .?accountId
                (hash-put! id-to-user .accountId (or short .?displayName))))))))))

(def (name-to-id config name)
  "Resolve a username to an account ID"
  (make-user-hashes config)
  (hash-get user-to-id name))

(def (id-to-name config account-id)
  "Resolve an account ID to a username"
  (make-user-hashes config)
  (hash-get id-to-user account-id))

;;;; Groups

(def (group-list config (query ""))
  "List groups"
  (let ((body (jira-get config (format "/rest/api/3/groups/picker?query=~a&maxResults=50" query)))
        (headers ["Name" "Group ID"])
        (rows []))
    (when (hash-table? body)
      (let-hash body
        (when (and .?groups (list? .groups))
          (for (g .groups)
            (when (hash-table? g)
              (let-hash g
                (set! rows (cons [(or .?name "") (or .?groupId "")] rows))))))))
    (output headers (reverse rows))))

(def (group-members config group-name)
  "List members of a group"
  (let ((body (jira-get config (format "/rest/api/3/group/member?groupname=~a&includeInactiveUsers=false" group-name)))
        (headers ["Name" "Email" "Active" "Account Type"])
        (rows []))
    (when (hash-table? body)
      (let-hash body
        (when (and .?values (list? .values))
          (for (m .values)
            (when (hash-table? m)
              (let-hash m
                (set! rows (cons [(or .?displayName "")
                                  (or .?emailAddress "")
                                  (yon .?active)
                                  (or .?accountType "")]
                                 rows))))))))
    (output headers (reverse rows))))

;;;; Permissions

(def (my-permissions config)
  "Get current user permissions"
  (let ((body (jira-get config "/rest/api/3/mypermissions")))
    (when (hash-table? body)
      (let-hash body
        (when (hash-table? .?permissions)
          (let ((headers ["Permission" "Name" "Have"])
                (rows []))
            (hash-for-each
             (lambda (k v)
               (when (hash-table? v)
                 (set! rows (cons [(->string k)
                                   (or (hash-get v 'name) "")
                                   (yon (hash-get v 'havePermission))]
                                  rows))))
             .permissions)
            (output headers (reverse rows))))))))
