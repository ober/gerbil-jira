#!/usr/bin/env gxi
;; -*- Gerbil -*-

(import :std/build-script)

(defbuild-script
  '("jira/client"
    (static-exe:
     "jira/jira"
     "-ld-options"
     "-lpthread -lyaml -ldl -lssl -lz -L/usr/lib64 -L/usr/pkg/lib -I/usr/pkg/include")))
