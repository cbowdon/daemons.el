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
  "Services commands alist for SysVinit.")

(defun services-sysvinit--parse-list-item (raw-chkconfig-output)
  "Parse a single line from RAW-CHKCONFIG-OUTPUT into a tabulated list item."
  (let* ((parts (split-string raw-chkconfig-output nil t))
         (name (car parts))
         (run-level-statuses (cdr parts)))
    (list name (apply 'vector (cons name run-level-statuses)))))

(defun services-sysvinit--list ()
  "Return a list of services on a SysVinit system."
  (thread-last "chkconfig --list"
    (services--shell-command-to-string)
    (split-lines)
    (seq-map 'services-sysvinit--parse-list-item)))

(defun services-sysvinit--list-headers ()
  "Return the list of headers for a SysVinit services-mode buffer."
  (apply 'vector
         (cons '("Service" 40 t)
               (seq-map
                (lambda (x)
                  (list (number-to-string x) 5 t))
                (number-sequence 0 6)))))

(setq services--commands-alist services-sysvinit--commands-alist
      services--list-fun 'services-sysvinit--list
      services--list-headers-fun 'services-sysvinit--list-headers)

;;; services-sysvinit.el ends here
