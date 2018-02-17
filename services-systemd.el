;;; services-systemd.el --- UI for managing init system services -*- lexical-binding: t -*-

;; Copyright (c) 2018 Chris Bowdon
;;
;; Author: Chris Bowdon
;; URL: https://github.com/cbowdon/services-mode
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3
;;
;;; Commentary:

;;; Code:
(require 'seq)
(require 'services)

(defvar services-systemd--commands-alist
  '((status . (lambda (name) (format "systemctl status %s" name)))
    (start . (lambda (name) (format "systemctl start %s" name)))
    (stop . (lambda (name) (format "systemctl stop %s" name)))
    (restart . (lambda (name) (format "systemctl restart %s" name)))
    (reload . (lambda (name) (format "systemctl reload %s" name))))
  "Services commands alist for systemd.")

(defun services-systemd--parse-list-item (raw-systemctl-output)
  "Parse a single line from RAW-SYSTEMCTL-OUTPUT into a tabulated list item."
  (let* ((parts (split-string raw-systemctl-output))
         (name (replace-regexp-in-string "\.service" "" (car parts)))
         (enabled (cadr parts)))
    (list name (vector name enabled))))

(defun services-systemd--list ()
  "Return a list of services on a systemd system."
  (thread-last  "systemctl list-unit-files --type=service --no-legend"
    (services--shell-command-to-string)
    (split-lines)
    (seq-map 'services-systemd--parse-list-item)))

(defun services-systemd--list-headers ()
  "Return the list of headers for a systemd services-mode buffer."
  [("Service" 60 t)
   ("Enabled" 40 t)])

(setq services--commands-alist services-systemd--commands-alist
      services--list-fun 'services-systemd--list
      services--list-headers-fun 'services-systemd--list-headers)

(provide 'services-systemd)
;;; services-systemd.el ends here
