#!/usr/bin/env gxi

(import :std/iter
        :std/format
        :std/test
        :std/generic
        :ober/oberlib
        "jira/client.ss")

(def (jira-test)
  (def (test-convert-names)
    (convert-names "This is a test for @janedoe to confluence"))
  (let ((results (test-convert-names))
        (expect "This is a test for [~janedoe] to confluence"))
    (if (string=? results expect)
      (displayln "pass")
      (display (format "fail: got: ~a~% expected: ~a~%" results expect)))))

    ;;(check-output (test-convert-names)

(jira-test)
