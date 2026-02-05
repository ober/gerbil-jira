;;; -*- Gerbil -*-
;;; Jira CLI — Configuration & Authentication

(import
  :clan/text/yaml
  :gerbil/gambit
  :std/crypto/cipher
  :std/format
  :std/misc/ports
  :std/sugar
  :std/text/base64
  :std/text/json)

(export #t)

(def version "0.50")
(def config-file "~/.jira.yaml")
(def keys-dir (path-expand "~/.config/gerbil/keys"))
(def key-file (path-expand "jira.key" keys-dir))

;;;; Key file management

(def (ensure-keys-dir!)
  (unless (file-exists? keys-dir)
    (create-directory* keys-dir)
    (let ((proc (open-process [path: "chmod" arguments: ["700" keys-dir]])))
      (process-status proc)
      (close-port proc))))

(def (save-key-to-file! key-bytes)
  (ensure-keys-dir!)
  (let ((key-b64 (u8vector->base64-string key-bytes)))
    (with-output-to-file [path: key-file create: 'maybe truncate: #t]
      (lambda () (display key-b64)))
    (let ((proc (open-process [path: "chmod" arguments: ["400" key-file]])))
      (process-status proc)
      (close-port proc))))

(def (load-key-from-file)
  (unless (file-exists? key-file)
    (error (format "Key file not found: ~a. Run 'jira config' first." key-file)))
  (base64-string->u8vector (read-file-string key-file)))

(def (check-file-permissions! file)
  (when (file-exists? file)
    (let* ((info (file-info file))
           (mode (file-info-mode info)))
      (when (> (bitwise-and mode #o077) 0)
        (displayln (format "WARNING: ~a has insecure permissions. Run: chmod 600 ~a" file file))))))

;;;; Auth helpers

(def (make-basic-auth user password)
  (format "Basic ~a" (u8vector->base64-string (string->bytes (format "~a:~a" user password)))))

(def (get-password iv password)
  (let ((key (load-key-from-file)))
    (bytes->string
     (decrypt
      (make-aes-256-ctr-cipher)
      key
      (base64-string->u8vector iv)
      (base64-string->u8vector password)))))

(def (get-password-legacy key iv password)
  (displayln "WARNING: Legacy config with key in config file.")
  (displayln "         Run 'jira config' to upgrade to secure key storage.")
  (bytes->string
   (decrypt
    (make-aes-256-ctr-cipher)
    (base64-string->u8vector key)
    (base64-string->u8vector iv)
    (base64-string->u8vector password))))

;;;; Config loading

(def (load-config)
  (check-file-permissions! config-file)
  (check-file-permissions! key-file)
  (let ((config (hash)))
    (hash-for-each
     (lambda (k v)
       (hash-put! config (string->symbol k) v))
     (car (yaml-load config-file)))
    (let-hash config
      (when .?secrets
        (let ((secrets-json (parameterize ((read-json-key-as-symbol? #t))
                              (with-input-from-string
                                  (bytes->string (base64-decode .secrets))
                                read-json))))
          (let-hash secrets-json
            (let ((password (if .?key
                              (get-password-legacy .key .iv .password)
                              (get-password .iv .password))))
              (hash-put! config 'basic-auth (make-basic-auth ..?user password)))))))
    config))

;;;; Interactive config setup

(def (config-setup!)
  (let-hash (load-config)
    (display "What is your Jira API key? (will not echo): ")
    (let* ((password (read-password ##console-port))
           (cipher (make-aes-256-ctr-cipher))
           (iv (random-bytes (cipher-iv-length cipher)))
           (key (random-bytes (cipher-key-length cipher)))
           (encrypted-password (encrypt cipher key iv password))
           (enc-pass-store (u8vector->base64-string encrypted-password))
           (iv-store (u8vector->base64-string iv))
           (secrets-hash (hash
                          ("password" enc-pass-store)
                          ("iv" iv-store)))
           (secrets (u8vector->base64-string (string->bytes (json-object->string secrets-hash)))))
      (save-key-to-file! key)
      (displayln "")
      (displayln "Add the following line to your " config-file)
      (displayln "")
      (displayln "secrets: " secrets)
      (displayln "")
      (displayln "Key saved to " key-file))))
