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
;; Created: February 13, 2018
;; Modified: February 13, 2018
;; Version: 0.0.1
;; Keywords: startup screen tools
;; Package-Requires: ((emacs "25.3") (page-break-lines "0.11"))
;;
;;; Commentary:
;; A UI for managing init system services.

;;; Code:


(require 'seq)

;; declarations
(defconst services--dashboard-buffer-name "*services*")
(defconst services--output-buffer-name "*services-output*")
(defconst services--error-buffer-name "*services-error*")

(defcustom services-always-sudo nil
  "Whether to always attempt to sudo up in services-mode."
  :type 'boolean)

;; to be defined for each init system
(defvar services--commands-alist nil "Services commands alist")
(defvar services--list-fun nil "Function to list all services")
(defvar services--pretty-print-fun nil "Function to list all services")

(defvar services-mode-map nil "Keymap for services mode")

(defvar services-list nil "List of current system services")

;; defuns
(defun split-lines (string)
  "Split STRING Into list of lines"
  (split-string string "[\n\r]+" t))

(defun services--list-all ()
  (funcall services--list-fun))

(defun services--pretty-print (service)
  (funcall services--pretty-print-fun service))

(defun services-next-line ()
  "Move the cursor the next line"
  (interactive)
  (beginning-of-line 2))

(defun services-prev-line ()
  "Move the cursor the prev line"
  (interactive)
  (beginning-of-line 0))

(defun services--service-at-point ()
  (let ((index (- (line-number-at-pos) (services--header-lines) 1)))
    (nth index services-list)))

(defun services-run (command)
  "Run the given service COMMAND. Show results in a temporary buffer or the minibuffer."
  (let ((service-name (car (services--service-at-point)))
        (command-fun (alist-get command services--commands-alist)))
    (when (not command-fun)
      (error "No such service command: %s" command))
    (async-shell-command (funcall command-fun service-name)
                         (get-buffer-create services--output-buffer-name)
                         (get-buffer-create services--error-buffer-name))))

(defun services-status-at-point () (interactive) (services-run 'status))
(defun services-show-at-point () (interactive) (services-run 'show))
(defun services-start-at-point () (interactive) (services-run 'start))
(defun services-stop-at-point () (interactive) (services-run 'stop))
(defun services-restart-at-point () (interactive) (services-run 'restart))
(defun services-reload-at-point () (interactive) (services-run 'reload))

;; dashboard drawing
(defun services--header ()
  (list
   (format "Services on %s (%s)" system-name (current-time-string))
   ""))

(defun services--header-lines ()
  (length (services--header)))

(defun services-refresh-dashboard ()
  (interactive)
  ;; clean up
  (save-excursion
    (delete-region (point-min) (point-max))
    ;; insert header
    (dolist (header-line (services--header))
      (insert (format "%s\n" header-line)))
    ;; insert contents
    (setq services-list (services--list-all))
    (dolist (service services-list)
      (insert (services--pretty-print service)))))

;; Start by supporting systemd
(load-file "./services-systemd.el")

;; assignments
(setq services-mode-map
      (let ((map (make-sparse-keymap)))
        (define-key map [tab] 'services-next-line)
        (define-key map [backtab] 'services-prev-line)
        (define-key map (kbd "g") 'services-refresh-dashboard)
        (define-key map (kbd "n") 'services-next-line)
        (define-key map (kbd "p") 'services-prev-line)
        (define-key map (kbd "RET") 'services-status-at-point)
        (define-key map (kbd "s") 'services-start-at-point)
        (define-key map (kbd "S") 'services-stop-at-point)
        (define-key map (kbd "R") 'services-restart-at-point)
        (define-key map (kbd "r") 'services-reload-at-point)
        map))

;; mode definition
(define-derived-mode services-mode special-mode
  "Services"
  "Dashboard for viewing and controlling system services"
  (linum-mode -1)
  (page-break-lines-mode 1)
  (whitespace-mode -1)
  (setq buffer-read-only nil
        truncate-lines t)
  (when services-always-sudo
    ;; Become root, but hang out in a temp dir to minimise damage potential
    (let ((tempdir (shell-command-to-string "mktemp -d")))
      (cd (format "/sudo::%s "tempdir)))))

(defun services ()
  (interactive)
  (let ((dashboard-buffer (get-buffer-create services--dashboard-buffer-name)))
    (with-current-buffer dashboard-buffer
      (display-buffer-pop-up-window dashboard-buffer nil)
      (switch-to-buffer-other-window dashboard-buffer)
      (services-mode)
      (services-refresh-dashboard))))

;; evil
(when (and (boundp 'evil-emacs-state-modes)
           (not (memq 'services-mode evil-emacs-state-modes)))
  (add-to-list 'evil-emacs-state-modes 'services-mode))
