#!/usr/bin/env gxi
;; -*- Gerbil -*-

(import :std/build-script)

(defbuild-script
  '("jira/client"
    (exe:
     "jira/jira"
     "-ld-options"
     "-lyaml")))
