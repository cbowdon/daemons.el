;;; services.el --- UI for managing init system services -*- lexical-binding: t -*-

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
(defvar services--commands-alist-systemd nil "Services commands alist for systemd")

(setq services--commands-alist-systemd
      '((show . (lambda (name) (format "systemctl show %s" name)))
        (status . (lambda (name) (format "systemctl status %s" name)))
        (start . (lambda (name) (format "systemctl start %s" name)))
        (stop . (lambda (name) (format "systemctl stop %s" name)))
        (restart . (lambda (name) (format "systemctl restart %s" name)))
        (reload . (lambda (name) (format "systemctl reload %s" name)))))

(defun services--systemd-parse-list (raw-systemctl-output)
  (seq-map
   (lambda (line)
     (let* ((parts (split-string line))
            (name (replace-regexp-in-string "\.service" "" (car parts)))
            (enabled (cadr parts)))
       (list name (vector name enabled))))
   raw-systemctl-output))

(defun services--systemd-list-all ()
  "Return an alist of services on a systemd system.
  The car of each cons pair is the service name.
  The cdr is a plist of extended properties (e.g. enabled/disabled status)."
  (thread-last  "systemctl list-unit-files --type=service --no-legend"
    (shell-command-to-string)
    (split-lines)
    (services--systemd-parse-list)))

(setq services--commands-alist services--commands-alist-systemd)
(setq services--list-fun 'services--systemd-list-all)
