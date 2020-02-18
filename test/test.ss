#!/usr/bin/env gxi

(import :std/iter
        :std/format
        :std/test
        :std/generic
        :ober/oberlib
        "../jira/client.ss")

(def (jira-test name got expect)
  (displayln "#### Test: " name)
  (if (string=? got expect)
    (displayln "pass: ")
    (display (format "fail: got:-> ~a~% expected:-> ~a~%" got expect))))

(def (jira-tests)
  (jira-test "convert-name: middle" (convert-names "This is a test for @janedoe to confluence") "This is a test for [~janedoe] to confluence")
  (jira-test "convert-name: beginning" (convert-names "@janedoe this is a test for confluence") "[~janedoe] this is a test for confluence"))

(jira-tests)
