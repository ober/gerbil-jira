#!/usr/bin/env gxi
;; -*- Gerbil -*-

(import :std/build-script)

(defbuild-script
  '("jira/client"
    (static-exe:
     "jira/jira")))
