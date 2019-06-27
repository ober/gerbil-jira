;;; -*- lexical-binding: t -*-
;;; jira.el --- Jira helper functions for gerbil-jira

;; Copyright (C) 2019 Jaime Fournier <jaimef@linbsd.org>

;; Author: Jaime Fournier <jaimef@linbsd.org>
;; Keywords: Jira helper functions for gerbil-jira
;; Version: 0.1

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

(defvar *jira-server* "jira.example.com")
(defvar *jira-bin* "/usr/local/bin/jira")
(defvar *jira-dir* "~/jiras/")

(defun jira-map ()
  "Map used for interactive jira keys"
  (interactive)
  (lexical-let ((map (make-sparse-keymap)))
    (define-key map (kbd "<RET>") #'(lambda () (interactive "p") (show-issue)))))

(defun jira-search (m)
  "Search for pattern in Jira, and return all tickets that match"
  (interactive "sPattern: ")
  (let* ((clean (clean-buffer-name m))
	 (buffer (format "jira-search-%s" clean))
	 (out (format "/tmp/jira-search-%s" clean)))
    (call-process-shell-command (format "%s search '%s' > %s" *jira-bin* m out))
    (find-file out)
    (switch-to-buffer buffer)
    (org-mode)
    (org-cycle)
    (save-buffer)))

(defun jira-q (m)
  "Run custom queries defined in ~/.jira.yaml"
  (interactive "sQuery: ")
  (let* ((clean (clean-buffer-name m))
	 (buffer (format "jira-q-%s" clean))
	 (out (format "/tmp/jira-q-%s" clean)))
    (call-process-shell-command (format "%s q '%s' > %s" *jira-bin* m out))
    (find-file out)
    (switch-to-buffer buffer)
    (org-mode)
    (org-cycle)))

(defun jira-issues ()
  (interactive)
  (call-process-shell-command (format "%s q myopen > %s/myissues.org" *jira-bin* *jira-dir*))
  ;;  (sleep-for 6)
  (find-file (format "%s/myissues.org" *jira-dir*))
  (org-mode)
  (org-cycle)
  (save-buffer))

(defun dump-opened ()
  (interactive)
  (call-process-shell-command (format "%s search 'reporter = currentUser() order by created DESC' > %s/myopened.org" *jira-bin* *jira-dir*))
  ;;  (sleep-for 6)
  (find-file (format "%s/myopened.org" *jira-dir*)))

(defun dump-recently-views ()
  (interactive)
  (call-process-shell-command (format "%s search \"issuekey in issueHistory() order by lastViewed DESC\" > %s/recentlyviewed.org" *jira-bin* *jira-dir*))
  (sleep-for 6)
  (find-file (format "%s/recentlyviewed.org" *jira-dir*)))

(defun dump-all-issues ()
  (interactive)
  (call-process-shell-command (format "%s search \"assignee = currentUser() order by updated DESC\" > %s/allmyissues.org" *jira-bin* *jira-dir*))
  (sleep-for 6)
  (find-file (format "%s/allmyissues.org" *jira-dir*)))

(defun get-issue-name ()
  (let ((issue (org-table-get-field 1)))
    (replace-regexp-in-string " " "" issue)))

(defun jira-new-issue (component summary description)
  (interactive "sComponent:\nsSummary:\nsDescription:")
  (let ((cmd (format "%s create \"%s\" \"%s\" \"%s\"" *jira-bin* component summary description)))
    (message cmd)
    (call-process-shell-command cmd)))

(defun get-issue (issue)
  (interactive "sIssue?")
  (let* ((buffer (format "jira-issue-%s" issue))
	 (out (format "%s/jira-issue-%s" issue *jira-dir*)))
    (call-process-shell-command (format "%s issue %s > %s" *jira-bin* issue out))
    (find-file out)
    (switch-to-buffer buffer)
    (org-mode)
    (org-cycle)))

(defun show-issue ()
  (interactive)
  (let* ((issue (get-issue-name))
	 (buffer (format "jira-issue-%s" issue))
	 (out (format "%s/jira-issue-%s" *jira-dir* issue)))
    (call-process-shell-command (format "%s issue %s 2>&1 > %s" *jira-bin* issue out))
    (find-file out)
    (switch-to-buffer buffer)
    (delete-trailing-whitespace)
    (visual-line-mode)
    (save-buffer)
    (org-mode)
    (org-cycle)))

(defun j-issues-I-Updated-in-last-week ()
  (interactive)
  (let* ((query "updated >= -2w and issuekey in issueHistory() order by lastViewed DESC")
	 (buffer "jira-issues-past-week-I-viewed-and-touched")
	 (out (format "/tmp/%s" buffer))
	 (cmd (format "%s search \"%s\"" *jira-bin* query)))
    (call-process-shell-command (format "%s > %s" cmd out))
    (find-file out)
    (switch-to-buffer buffer)
    (org-mode)
    (org-cycle)))

(defun j-issues-I-viewed-last ()
  (interactive)
  (let* ((query "issuekey in issueHistory() ORDER BY lastViewed DESC")
	 (buffer "jira-issues-commented-on-by-me")
	 (out (format "/tmp/%s" buffer))
	 (cmd (format "%s search \"%s\"" *jira-bin* query)))
    (call-process-shell-command (format "%s > %s" cmd out))
    (find-file out)
    (switch-to-buffer buffer)
    (org-mode)
    (org-cycle)))

(defun org-display-generic (cmd file)
  (call-process-shell-command (format "%s > %s" cmd file))
  (find-file file)
  (delete-trailing-whitespace)
  (save-buffer)
  (org-mode)
  (org-cycle))

(defun meta-issue ()
  (interactive)
  (let* ((issue (get-issue-name))
	 (buffer (format "jira-meta-%s" issue))
	 (out (format "/tmp/jira-meta-%s" issue)))
    (call-process-shell-command (format "%s metadata %s > %s" *jira-bin* issue out))
    (find-file out)
    (switch-to-buffer buffer)
    (org-mode)
    (org-cycle)))

(defun dump-all-issues ()
  (interactive)
  (call-process-shell-command (format "%s search \"assignee = currentUser() order by updated DESC\" > %s/allmyissues.org" *jira-bin* *jira-dir*))
  (sleep-for 6)
  (find-file (format "%s/allmyissues.org" *jira-dir*)))

(defun jtracking ()
  (interactive)
  (jira-search "labels = j-tracking"))

(defun j-search-label (label)
  "Search for all jira issues that have label"
  (interactive "sLabel")
  (jira-search (format "labels = %s" label)))

(defun j-label (label)
  "Tag a jira issue with tag/label"
  (interactive "sLabel:")
  (let* ((issue (get-issue-name))
	 (cmd (format "%s label %s %s" *jira-bin* issue label)))
    (call-process-shell-command cmd)))

(defun get-issue-name ()
  (let ((issue (org-table-get-field 1)))
    (replace-regexp-in-string " " "" issue)))

(defun jira-issue (issue)
  (interactive "sIssue:")
  (call-process-shell-command (format "open https://%s/browse/%s" *jira-server* issue)))

(defun j-view-issue ()
  (interactive)
  (let ((issue (get-issue-name)))
    (call-process-shell-command (format "open https://%s/browse/%s" *jira-server* issue))))
