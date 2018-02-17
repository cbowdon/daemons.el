;; services.el --- UI for managing init system services -*- lexical-binding: t -*-

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
;; Package-Requires: ((emacs "25.3")
;;
;;; Commentary:
;; A UI for managing init system services.

;;; Code:


(require 'seq)

;; declarations
(defconst services--list-buffer-name "*services*")
(defconst services--output-buffer-name "*services-output*")

;; customization
(defgroup services-mode-customization-group nil
  "Customization group for Services mode")

(defcustom services-always-sudo nil
  "Whether to always attempt to sudo up in services-mode.
This defaults to off because in some systems at least you can query status
without special privileges and will be prompted for a root password if you try
anything else.  But at other times it's much more convenient to just assume sudo
powers when the buffer loads and enact everything as root.

Security wise - off is safer of course, to avoid unnecessary privilege."
  :type 'boolean
  :group 'services-mode-customization-group)

(defcustom services-do-no-evil t
  "Whether to add services-mode(s) to evil-emacs-state-modes.
This is the author's preference - it's a special mode and these are ergonomic
enough that it's not worth choosing new bindings.  But the choice is yours."
  :type 'boolean
  :group 'services-mode-customization-group)

(defcustom services-init-system-submodule-path nil
  "Path to a Lisp file that implements specific commands for an init system.
e.g. \"./services-systemd.el\".
Those specific commands are:

- `services--commands-alist'
- `services--list-fun'
- `services--list-headers-fun'

If this variable is nil then the init system will be guessed by `services-guess-init-system-submodule-path'."
  :type '(file :must-match t)
  :group 'services-mode-customization-group)

(defvar services--shell-command-fun 'shell-command
  "Contains a `shell-command' function.

Override this to your own value for mocking out shell calls in tests.")

(defvar services-shell-command-to-string-fun 'shell-command-to-string
  "Contains a `shell-command-to-string' function.
Override this to your own value for mocking out shell calls in tests.")

;; to be defined for each init system
(defvar services--commands-alist nil
  "Services commands alist.
The car of each pair is the command symbol (e.g. 'stop).
The cdr of each pair is a function taking a service name and returning a shell
command to execute.

e.g. '((start . (lambda (x) (format \"service %s start\" x)))
       (stop . (lambda (x) (format \"service %s stop\" x))))")

(defvar services--list-fun nil
  "Function to list all services.
It should take no arguments and return a list in the right format for
`tabulated-list-entries'.
It will therefore also need to match the columns defined with
`services--list-headers-fun'.")

(defvar services--list-headers-fun nil
  "Function to get headers for list of all services.
It should take no arguments and return a vector in the right format for
`tabulated-list-format'.
It will therefore also need to match the entries returned by `services--list-fun'.")

(defvar services-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "RET") 'services-status-at-point)
    (define-key map (kbd "s") 'services-start-at-point)
    (define-key map (kbd "S") 'services-stop-at-point)
    (define-key map (kbd "R") 'services-restart-at-point)
    (define-key map (kbd "r") 'services-reload-at-point)
    map)
  "Keymap for services mode.")

(defvar services-output-mode-map services-mode-map "Keymap for services output mode.")
(defvar services--current-id nil "Current service id.")

;; defuns
(defun split-lines (string)
  "Split STRING Into list of lines."
  (split-string string "[\n\r]+" t))

(defun services--list ()
  "Return the list of all services."
  (funcall services--list-fun))

(defun services--list-headers ()
  "Return the headers for the list of all services."
  (funcall services--list-headers-fun))

(defun services--shell-command (&rest args)
  "Dynamically bound alias for `shell-command' (to enable test mocks).
ARGS are passed to the underlying function."
  (apply services--shell-command-fun args))

(defun services--shell-command-to-string (&rest args)
  "Dynamically bound alias for `shell-command-to-string' (to enable test mocks).
ARGS are passed to the underlying function."
  (apply services--shell-command-to-string-fun args))

(defun services--service-at-point ()
  "Return the id of the service of the current line if in the list buffer.
Otherwise, return value of services--current-id variable (set by services--run)."
  (if (derived-mode-p 'tabulated-list-mode)
      (tabulated-list-get-id)
    services--current-id))

(defun services--run (command)
  "Run the given service COMMAND.  Show results in a temporary buffer."
  (let ((service-name (services--service-at-point))
        (command-fun (alist-get command services--commands-alist)))
    (when (not command-fun)
      (error "No such service command: %s" command))
    (with-current-buffer (get-buffer-create services--output-buffer-name)
      (setq buffer-read-only nil
            services--current-id service-name)
      (delete-region (point-min) (point-max))
      (insert (concat
               (propertize (format "Output of `%s` on `%s`:" command service-name) 'face 'underline)
               "\n\n"))
      (services--shell-command (funcall command-fun service-name) t)
      (services-output-mode))
    (when (not (equal (buffer-name) services--output-buffer-name))
      (switch-to-buffer-other-window services--output-buffer-name))))

(defun services-status-at-point ()
  "Show the status of the service at point in the services buffer."
  (interactive)
  (services--run 'status))

(defun services-start-at-point ()
  "Start the service at point in the services buffer."
  (interactive)
  (services--run 'start))

(defun services-stop-at-point ()
  "Stop the service at point in the services buffer."
  (interactive)
  (services--run 'stop))

(defun services-restart-at-point ()
  "Restart the service at point in the services buffer."
  (interactive)
  (services--run 'restart))

(defun services-reload-at-point ()
  "Reload the service at point in the services buffer."
  (interactive)
  (services--run 'reload))

;;;; To demo SysVinit support with mocked-out shell commands:
;; (setq services--init-system-submodule-path "./services-sysvinit.el")
;; (setq services--shell-command-to-string-fun (lambda (_) "
;; NetworkManager  0:off   1:off   2:on    3:on    4:on    5:on    6:off
;; abrt-ccpp       0:off   1:off   2:off   3:on    4:off   5:on    6:off
;; abrt-oops       0:off   1:off   2:off   3:on    4:off   5:on    6:off"))
;; (setq services--shell-command-fun (lambda (&rest _) (insert "service is fucking ded")))

(defun services-guess-init-system-submodule-path ()
  "Call \"which\" to identify an installed init system."
  (cond ((= 0 (services--shell-command "which systemctl")) "./services-systemd.el")
        ((= 0 (services--shell-command "which service")) "./services-sysvinit.el")
        (t (error "I'm sorry, your init system isn't supported yet!"))))

(load-file (or services-init-system-submodule-path (services-guess-init-system-submodule-path)))

;; mode definitions
(defun services-mode-refresh ()
  "Refresh the list of services."
  (setq tabulated-list-entries 'services--list))

(define-derived-mode services-mode tabulated-list-mode
  "Services"
  "UI for viewing and controlling system services"
  :group 'services-mode-customization-group
  (setq tabulated-list-format (services--list-headers)
        tabulated-list-padding 2)
  (add-hook 'tabulated-list-revert-hook 'services-mode-refresh)
  (tabulated-list-init-header))

(defun services ()
  "Open the list of system services for user management.

This opens a services-mode list buffer.  Move the cursor to a service line and
execute one of the commands in `describe-mode' to show status and manage the
state of the service."
  (interactive)
  (let ((list-buffer (get-buffer-create services--list-buffer-name)))
    (with-current-buffer list-buffer
      (display-buffer-pop-up-window list-buffer nil)
      (switch-to-buffer-other-window list-buffer)
      (when services-always-sudo
        ;; Become root, but hang out in a temp dir to minimise damage potential
        (let ((tempdir (services--shell-command-to-string "mktemp -d")))
          (cd (format "/sudo::%s" tempdir))))
      (services-mode)
      (services-mode-refresh)
      (tabulated-list-print t t))))

(define-derived-mode services-output-mode special-mode
  "Services Output"
  "Mode for displaying output of Services commands"
  :group 'services-mode-customization-group)

;; evil
(when (and services-do-no-evil
           (boundp 'evil-emacs-state-modes)
           (not (memq 'services-mode evil-emacs-state-modes)))
  (add-to-list 'evil-emacs-state-modes 'services-mode)
  (add-to-list 'evil-emacs-state-modes 'services-output-mode))

(provide 'services)
;;; services.el ends here
