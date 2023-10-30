;;; daemons-systemd.el --- UI for managing init system daemons (services) -*- lexical-binding: t -*-

;; Copyright (c) 2018 Chris Bowdon
;;
;; Author: Chris Bowdon
;; URL: https://github.com/cbowdon/daemons.el
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3
;;
;; Created: February 13, 2018
;; Modified: December 15, 2018
;; Version: 2.1.0
;; Keywords: unix convenience
;; Package-Requires: ((emacs "25.1"))
;;
;;; Commentary:
;; This file provides systemd support for daemons.el.

;;; Code:
(require 'seq)
(require 'daemons)

(defcustom daemons-systemd-is-user nil
  "Determines if we want to run commands as `--user'"
  :type 'boolean
  :group 'daemons)

(defcustom daemons-systemctl-command-fn #'daemons-systemctl-cmd
  "Function used to return string systemctl commands suitable for `shell-command'.

It should take two string input arguments, denoting the desired systemctl
command, and the service to run the command with.

The default value for this user option will respect the value of
`daemons-systemd-is-user'.  In order to also do this for any custom value of
this option, see the implementation of `daemons-systemd--cmd'."
  :type 'function
  :group 'daemons)

(defun daemons-systemd--cmd ()
  "Appends `--user' to the `systemctl' call if `daemons-systemd-is-user' is set"
  (if daemons-systemd-is-user
      "systemctl --user"
    "systemctl"))

(defun daemons--systemd-documentation-for (service)
  "Return documentation for SERVICE."
  (let* ((output (shell-command-to-string
                  (format "systemctl show %s --no-pager" service)))
         (lines (split-string output "\n"))
         (prefix "Description=")
         (desc (seq-find (lambda (s) (string-prefix-p prefix s)) lines)))
    (when desc
      (substring desc (length prefix)))))

(defun daemons--systemd-eldoc-function (callback &rest _ignored)
  "Document service at point by calling CALLBACK."
  (when-let* ((maybe-service (thing-at-point 'symbol))
              (line (thing-at-point 'line))
              ((string= maybe-service (car (split-string (string-trim line)))))
              (result (daemons--systemd-documentation-for maybe-service)))
    (funcall callback result
             :thing maybe-service
             :face 'font-lock-variable-name-face)))

(defun daemons-systemctl-cmd (command service)
  "Return a string suitable for `shell-command' for COMMAND run with SERVICE.

COMMAND should be a valid systemctl command, and SERVICE an existing systemd
service.  Both should be strings.

\"--user\" will be appended to the systemctl call if `daemons-systemd-is-user' is set."
  (format "%s %s %s" (daemons-systemd--cmd) command service))

(daemons-define-submodule daemons-systemd
  "Daemons submodule for systemd."

  :test (and (eq system-type 'gnu/linux)
             (equal 0 (daemons--shell-command "which systemctl")))
  :commands
  '((status . (lambda (name) (funcall daemons-systemctl-command-fn "status" name)))
    (start . (lambda (name) (funcall daemons-systemctl-command-fn "start" name)))
    (stop . (lambda (name) (funcall daemons-systemctl-command-fn "stop" name)))
    (restart . (lambda (name) (funcall daemons-systemctl-command-fn "restart" name)))
    (reload . (lambda (name) (funcall daemons-systemctl-command-fn "reload" name)))
    (enable . (lambda (name) (funcall daemons-systemctl-command-fn "enable" name)))
    (disable . (lambda (name) (funcall daemons-systemctl-command-fn "disable" name))))

  :list (daemons-systemd--list)

  :headers [("Daemon (service)" 60 t) ("Enabled" 40 t)]

  :eldoc-documentation-function #'daemons--systemd-eldoc-function)

(defun daemons-systemd--parse-list-item (raw-systemctl-output)
  "Parse a single line from RAW-SYSTEMCTL-OUTPUT into a tabulated list item."
  (let* ((parts (split-string raw-systemctl-output))
         (name (replace-regexp-in-string "\.service" "" (car parts)))
         (enabled (cadr parts)))
    (list name (vector name enabled))))

(defun daemons-systemd--item-is-simple-service-p (item)
  "Return non-nil if ITEM is not a template service.
ITEM should correspond to the output type of `daemons-systemd--parse-list-item'."
  (not (string-match-p "@$" (car item))))

(defun daemons-systemd--list ()
  "Return a list of daemons on a systemd system."
  (thread-last  (format "%s list-unit-files --type=service --no-legend" (daemons-systemd--cmd))
    (daemons--shell-command-to-string)
    (daemons--split-lines)
    (seq-map 'daemons-systemd--parse-list-item)
    (seq-filter 'daemons-systemd--item-is-simple-service-p)))

(defun daemons-systemd-toggle-user ()
  "Toggle showing of user services"
  (interactive)
  (setq daemons-systemd-is-user (not daemons-systemd-is-user))
  (revert-buffer))

(provide 'daemons-systemd)
;;; daemons-systemd.el ends here
