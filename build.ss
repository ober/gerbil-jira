#!/usr/bin/env gxi
;; -*- Gerbil -*-

(import :std/build-script)

(defbuild-script
  '("jira/config"
    "jira/api"
    "jira/format"
    "jira/users"
    "jira/issues"
    "jira/links"
    "jira/properties"
    "jira/search"
    "jira/projects"
    "jira/agile"
    "jira/admin"
    (exe:
     "jira/jira"
     "-ld-options"
     "-lyaml")))
