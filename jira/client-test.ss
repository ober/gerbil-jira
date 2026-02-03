;; -*- Gerbil -*-
;;; Jira client unit tests

(import
  :std/test
  :std/format
  :std/text/json
  :std/pregexp
  :ober/jira/client)

(export client-test)

(def client-test
  (test-suite "jira client"

    ;;; --- email-short ---
    (test-case "email-short: extracts username from email"
      (check (email-short "me@example.com") => "me"))

    (test-case "email-short: handles complex email"
      (check (email-short "john.doe@company.co.uk") => "john.doe"))

    (test-case "email-short: returns input when no @"
      (check (email-short "noemail") => "noemail"))

    (test-case "email-short: handles #f"
      (check (email-short #f) => #f))

    (test-case "email-short: handles empty string"
      (check (email-short "") => ""))

    ;;; --- convert-names ---
    (test-case "convert-names: converts @user in middle"
      (check (convert-names "test for @janedoe to confluence")
             => "test for [~janedoe] to confluence"))

    (test-case "convert-names: converts @user at beginning"
      (check (convert-names "@janedoe this is a test")
             => "[~janedoe] this is a test"))

    (test-case "convert-names: converts multiple @users"
      (check (convert-names "@alice and @bob are here")
             => "[~alice] and [~bob] are here"))

    (test-case "convert-names: no change without @"
      (check (convert-names "no users here") => "no users here"))

    ;;; --- text-to-adf ---
    (test-case "text-to-adf: returns hash table"
      (check (hash-table? (text-to-adf "Hello")) => #t))

    (test-case "text-to-adf: has doc type"
      (check (hash-ref (text-to-adf "Hello") "type") => "doc"))

    (test-case "text-to-adf: has version 1"
      (check (hash-ref (text-to-adf "Hello") "version") => 1))

    (test-case "text-to-adf: has content list"
      (check (list? (hash-ref (text-to-adf "Hello") "content")) => #t))

    (test-case "text-to-adf: content has paragraph"
      (let* ((adf (text-to-adf "Hello"))
             (content (hash-ref adf "content"))
             (para (car content)))
        (check (hash-ref para "type") => "paragraph")))

    (test-case "text-to-adf: paragraph has text node"
      (let* ((adf (text-to-adf "Hello"))
             (content (hash-ref adf "content"))
             (para (car content))
             (inner (hash-ref para "content"))
             (text-node (car inner)))
        (check (hash-ref text-node "type") => "text")
        (check (hash-ref text-node "text") => "Hello")))

    ;;; --- adf-to-text ---
    ;; adf-to-text uses symbol keys (from JSON parsing), not string keys
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

    ;;; --- org-table-safe ---
    (test-case "org-table-safe: replaces pipe characters"
      (check (org-table-safe "a|b|c") => "a-b-c"))

    (test-case "org-table-safe: no change without pipes"
      (check (org-table-safe "no pipes") => "no pipes"))

    (test-case "org-table-safe: handles non-string"
      (check (org-table-safe 42) => 42))

    (test-case "org-table-safe: handles empty string"
      (check (org-table-safe "") => ""))

    ;;; --- default-headers ---
    (test-case "default-headers: returns list of 3 headers"
      (check (length (default-headers "Basic abc")) => 3))

    (test-case "default-headers: contains authorization"
      (let ((headers (default-headers "Basic mytoken")))
        (check (list? headers) => #t)
        ;; Last header should be authorization
        (let ((auth-header (caddr headers)))
          (check (car auth-header) => "Authorization"))))

    ;;; --- red ---
    (test-case "red: wraps text in ANSI red codes"
      (let ((result (red "error")))
        (check (string? result) => #t)))

    (test-case "red: contains original text"
      (let ((result (red "warning")))
        (check (string-contains result "warning") ? values)))

    (test-case "red: contains ANSI escape"
      (let ((result (red "test")))
        (check (string-contains result "\x1b;") ? values)))

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

    ;;; --- program-name ---
    (test-case "program-name: is jira"
      (check program-name => "jira"))
    ))
