;;; services-sysvinit.el --- UI for managing init system services -*- lexical-binding: t -*-

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

(defvar services-sysvinit--commands-alist
  '((status . (lambda (name) (format "service %s status" name)))
    (start . (lambda (name) (format "service %s start" name)))
    (stop . (lambda (name) (format "service %s stop" name)))
    (restart . (lambda (name) (format "service %s restart" name)))
    (reload . (lambda (name) (format "service %s reload" name))))
  "Services commands alist for SysVinit")

(defun services-sysvinit--parse-list-item (raw-chkconfig-output)
  (let* ((parts (split-string raw-chkconfig-output nil t))
         (name (car parts))
         (run-level-statuses (cdr parts)))
    (list name (apply 'vector run-level-statuses))))

(defun services-sysvinit--list ()
  "Return a list of services on a SysVinit system."
  (thread-last "chkconfig --list"
    (shell-command-to-string)
    (split-lines)
    (services-sysvinit--parse-list-item)))

(defun services-sysvinit--list-headers ()
  (apply 'vector
         (cons '("Service" 60 t)
               (seq-map
                (lambda (x)
                  (list (number-to-string x) 5 t))
                (number-sequence 0 6)))))

(setq services--commands-alist services-sysvinit--commands-alist
      services--list-fun 'services-sysvinit--list
      services--list-headers-fun 'services-sysvinit--list-headers)
