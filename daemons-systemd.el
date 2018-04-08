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
;; Modified: February 13, 2018
;; Version: 1.2.0
;; Keywords: unix convenience
;; Package-Requires: ((emacs "25"))
;;
;;; Commentary:
;; This file provides systemd support for daemons.el.

;;; Code:
(require 'seq)
(require 'daemons)

(defvar daemons-systemd--commands-alist
  '((status . (lambda (name) (format "systemctl status %s" name)))
    (start . (lambda (name) (format "systemctl start %s" name)))
    (stop . (lambda (name) (format "systemctl stop %s" name)))
    (restart . (lambda (name) (format "systemctl restart %s" name)))
    (reload . (lambda (name) (format "systemctl reload %s" name))))
  "Daemons commands alist for systemd.")

(defun daemons-systemd--parse-list-item (raw-systemctl-output)
  "Parse a single line from RAW-SYSTEMCTL-OUTPUT into a tabulated list item."
  (let* ((parts (split-string raw-systemctl-output))
         (name (replace-regexp-in-string "\.service" "" (car parts)))
         (enabled (cadr parts)))
    (list name (vector name enabled))))

(defun daemons-systemd--item-is-simple-service-p (item)
  "Non-nil if ITEM (output of `daemons-systemd--parse-list-item') is not a template service."
  (not (string-match-p "@$" (car item))))

(defun daemons-systemd--list ()
  "Return a list of daemons on a systemd system."
  (thread-last  "systemctl list-unit-files --type=service --no-legend"
    (daemons--shell-command-to-string)
    (daemons--split-lines)
    (seq-map 'daemons-systemd--parse-list-item)
    (seq-filter 'daemons-systemd--item-is-simple-service-p)))

(defun daemons-systemd--list-headers ()
  "Return the list of headers for a systemd ‘daemons-mode’ buffer."
  [("Daemon (service)" 60 t)
   ("Enabled" 40 t)])

(setq daemons--commands-alist daemons-systemd--commands-alist
      daemons--list-fun 'daemons-systemd--list
      daemons--list-headers-fun 'daemons-systemd--list-headers)

(provide 'daemons-systemd)
;;; daemons-systemd.el ends here
