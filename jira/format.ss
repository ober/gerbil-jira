;;; -*- Gerbil -*-
;;; Jira CLI — Output Formatting

(import
  :std/format
  :std/iter
  :std/misc/string
  :std/pregexp
  :std/srfi/13
  :std/sugar
  :std/text/csv
  :std/text/json)

(export #t)

;;;; Output format parameter

(def current-output-format (make-parameter 'table))

;;;; Helpers

(def (yon b)
  "Boolean to Yes/No string"
  (if b "Yes" "No"))

(def (->string v)
  "Convert any value to display string"
  (cond
   ((not v) "")
   ((string? v) v)
   ((number? v) (format "~a" v))
   ((boolean? v) (yon v))
   ((symbol? v) (symbol->string v))
   ((list? v) (string-join (map ->string v) ", "))
   ((hash-table? v) (json-object->string v))
   (else (format "~a" v))))

(def (truncate-string s max)
  "Truncate string to max length with ellipsis"
  (if (and (string? s) (> (string-length s) max))
    (string-append (substring s 0 (- max 3)) "...")
    (or s "")))

(def (email-short email)
  "Return the username part before @"
  (if (and email (string? email) (string-contains email "@"))
    (car (pregexp-split "@" email))
    (or email "")))

(def (date-short dt)
  "Shorten ISO datetime to date portion"
  (if (and dt (string? dt) (>= (string-length dt) 10))
    (substring dt 0 10)
    (or dt "")))

(def (org-table-safe str)
  "Escape pipe characters for table display"
  (if (string? str)
    (pregexp-replace* "\\|" str "/")
    (->string str)))

;;;; ADF (Atlassian Document Format) conversion

(def (text-to-adf text)
  "Convert plain text to Atlassian Document Format (ADF)"
  (hash
   ("type" "doc")
   ("version" 1)
   ("content" [(hash
                ("type" "paragraph")
                ("content" [(hash ("type" "text") ("text" text))]))])))

(def (adf-to-text adf)
  "Convert ADF to plain text"
  (cond
   ((string? adf) adf)
   ((not (hash-table? adf)) (if adf (format "~a" adf) ""))
   (else
    (let ((type (hash-get adf 'type))
          (content (hash-get adf 'content))
          (text (hash-get adf 'text)))
      (cond
       (text text)
       ((and content (list? content))
        (string-join
         (map adf-to-text content)
         (if (equal? type "paragraph") "\n" "")))
       (else ""))))))

;;;; Table output (plain text aligned columns)

(def (column-widths headers rows)
  "Calculate column widths from headers and data"
  (let ((widths (map string-length (map ->string headers))))
    (for (row rows)
      (set! widths
        (map max widths
             (map (lambda (cell) (string-length (->string cell))) row))))
    widths))

(def (pad-right s width)
  "Pad string to width with spaces on the right"
  (let* ((str (->string s))
         (len (string-length str)))
    (if (>= len width)
      str
      (string-append str (make-string (- width len) #\space)))))

(def (output-table headers rows)
  "Print aligned plain text table"
  (when (and headers (pair? headers))
    (let* ((str-headers (map ->string headers))
           (str-rows (map (lambda (row) (map ->string row)) rows))
           (widths (column-widths str-headers str-rows))
           (sep (string-join (map (lambda (w) (make-string w #\-)) widths) "-+-")))
      ;; Header
      (displayln (string-join (map pad-right str-headers widths) " | "))
      ;; Separator
      (displayln sep)
      ;; Rows
      (for (row str-rows)
        (displayln (string-join (map pad-right row widths) " | "))))))

;;;; JSON output

(def (output-json data)
  "Pretty-print data as JSON"
  (displayln (pretty-json data)))

;;;; CSV output

(def (output-csv headers rows)
  "Output data as CSV"
  (let ((port (current-output-port)))
    (write-csv-line (map ->string headers) port)
    (for (row rows)
      (write-csv-line (map ->string row) port))))

;;;; Main output dispatcher

(def (output headers rows)
  "Output data in the current format (table/json/csv)"
  (case (current-output-format)
    ((json)
     (let ((data (map (lambda (row)
                        (let ((h (hash)))
                          (for-each (lambda (header cell)
                                     (hash-put! h header cell))
                                   headers row)
                          h))
                      rows)))
       (output-json data)))
    ((csv) (output-csv headers rows))
    (else (output-table headers rows))))

;;;; Display a single item (key-value pairs)

(def (display-item label value)
  "Display a single labeled value"
  (displayln (format "~a: ~a" (pad-right label 15) (->string value))))
