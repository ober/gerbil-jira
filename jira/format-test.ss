;;; -*- Gerbil -*-
;;; Tests for format, api, and config modules

(import
  :std/test
  :std/format
  :std/text/json
  :jira/jira/format
  :jira/jira/api
  :jira/jira/config)

(export format-test)

(def format-test
  (test-suite "jira format/api/config"

    ;;; --- email-short ---
    (test-case "email-short: extracts username from email"
      (check (email-short "me@example.com") => "me"))

    (test-case "email-short: handles complex email"
      (check (email-short "john.doe@company.co.uk") => "john.doe"))

    (test-case "email-short: returns input when no @"
      (check (email-short "noemail") => "noemail"))

    (test-case "email-short: handles #f"
      (check (email-short #f) => ""))

    (test-case "email-short: handles empty string"
      (check (email-short "") => ""))

    ;;; --- yon ---
    (test-case "yon: true gives Yes"
      (check (yon #t) => "Yes"))

    (test-case "yon: false gives No"
      (check (yon #f) => "No"))

    ;;; --- ->string ---
    (test-case "->string: string passthrough"
      (check (->string "hello") => "hello"))

    (test-case "->string: number to string"
      (check (->string 42) => "42"))

    (test-case "->string: #f to empty"
      (check (->string #f) => ""))

    (test-case "->string: boolean to Yes/No"
      (check (->string #t) => "Yes"))

    (test-case "->string: symbol to string"
      (check (->string 'foo) => "foo"))

    ;;; --- truncate-string ---
    (test-case "truncate-string: short string unchanged"
      (check (truncate-string "hi" 10) => "hi"))

    (test-case "truncate-string: long string truncated"
      (check (truncate-string "hello world this is long" 10) => "hello w..."))

    (test-case "truncate-string: handles #f"
      (check (truncate-string #f 10) => ""))

    ;;; --- date-short ---
    (test-case "date-short: extracts date from ISO datetime"
      (check (date-short "2024-01-15T10:30:00.000+0000") => "2024-01-15"))

    (test-case "date-short: handles short string"
      (check (date-short "2024") => "2024"))

    (test-case "date-short: handles #f"
      (check (date-short #f) => ""))

    ;;; --- org-table-safe ---
    (test-case "org-table-safe: replaces pipe characters"
      (check (org-table-safe "a|b|c") => "a/b/c"))

    (test-case "org-table-safe: no change without pipes"
      (check (org-table-safe "no pipes") => "no pipes"))

    (test-case "org-table-safe: handles non-string"
      (check (org-table-safe 42) => "42"))

    (test-case "org-table-safe: handles empty string"
      (check (org-table-safe "") => ""))

    ;;; --- text-to-adf ---
    (test-case "text-to-adf: returns hash table"
      (check (hash-table? (text-to-adf "Hello")) => #t))

    (test-case "text-to-adf: has doc type"
      (check (hash-ref (text-to-adf "Hello") "type") => "doc"))

    (test-case "text-to-adf: has version 1"
      (check (hash-ref (text-to-adf "Hello") "version") => 1))

    (test-case "text-to-adf: has content list"
      (check (list? (hash-ref (text-to-adf "Hello") "content")) => #t))

    (test-case "text-to-adf: content has paragraph with text"
      (let* ((adf (text-to-adf "Hello"))
             (content (hash-ref adf "content"))
             (para (car content))
             (inner (hash-ref para "content"))
             (text-node (car inner)))
        (check (hash-ref para "type") => "paragraph")
        (check (hash-ref text-node "type") => "text")
        (check (hash-ref text-node "text") => "Hello")))

    ;;; --- adf-to-text ---
    (test-case "adf-to-text: handles plain string"
      (check (adf-to-text "plain text") => "plain text"))

    (test-case "adf-to-text: handles #f"
      (check (adf-to-text #f) => ""))

    (test-case "adf-to-text: handles number"
      (check (adf-to-text 42) => "42"))

    (test-case "adf-to-text: extracts text from ADF text node"
      (let ((h (make-hash-table)))
        (hash-put! h 'type "text")
        (hash-put! h 'text "Hello world")
        (check (adf-to-text h) => "Hello world")))

    (test-case "adf-to-text: extracts text from ADF paragraph"
      (let ((text-node (make-hash-table))
            (para (make-hash-table)))
        (hash-put! text-node 'type "text")
        (hash-put! text-node 'text "Content here")
        (hash-put! para 'type "paragraph")
        (hash-put! para 'content [text-node])
        (check (adf-to-text para) => "Content here")))

    (test-case "adf-to-text: extracts text from full ADF document"
      (let ((text-node (make-hash-table))
            (para (make-hash-table))
            (doc (make-hash-table)))
        (hash-put! text-node 'type "text")
        (hash-put! text-node 'text "Full doc")
        (hash-put! para 'type "paragraph")
        (hash-put! para 'content [text-node])
        (hash-put! doc 'type "doc")
        (hash-put! doc 'version 1)
        (hash-put! doc 'content [para])
        (check (adf-to-text doc) => "Full doc")))

    ;;; --- default-headers ---
    (test-case "default-headers: returns list of 3 headers"
      (check (length (default-headers "Basic abc")) => 3))

    (test-case "default-headers: contains authorization"
      (let ((headers (default-headers "Basic mytoken")))
        (check (list? headers) => #t)
        (let ((auth-header (caddr headers)))
          (check (car auth-header) => "Authorization")
          (check (cdr auth-header) => "Basic mytoken"))))

    ;;; --- version ---
    (test-case "version: is a string"
      (check (string? version) => #t))

    (test-case "version: is not empty"
      (check (string-length version) ? positive?))

    ;;; --- config-file ---
    (test-case "config-file: is set"
      (check (string? config-file) => #t))

    (test-case "config-file: ends with .yaml"
      (check (string-contains config-file ".yaml") ? values))
    ))
