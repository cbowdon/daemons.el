;;; daemons.el --- UI for managing init system daemons (services) -*- lexical-binding: t -*-

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
;; A UI for managing init system daemons (services).

;;; Code:
(require 'seq)

;; customization
(defgroup daemons nil
  "Customization group for Daemons mode"
  :group 'daemons)

(defcustom daemons-always-sudo nil
  "Whether to always attempt to sudo up in ‘daemons-mode’.
This defaults to off because in some systems at least you can query status
without special privileges and will be prompted for a root password if you try
anything else.  But at other times it's much more convenient to just assume sudo
powers when the buffer loads and enact everything as root.

Security wise - off is safer of course, to avoid unnecessary privilege."
  :type 'boolean
  :group 'daemons)

(defcustom daemons-init-system-submodule nil
  "Lisp module that implements specific commands for an init system.
e.g. 'daemons-systemd.
Those specific commands are:

- `daemons--commands-alist'
- `daemons--list-fun'
- `daemons--list-headers-fun'

If this variable is nil then the init system will be guessed by `daemons-guess-init-system-submodule'."
  :type 'symbol
  :group 'daemons)

;; declarations
(defvar daemons--shell-command-fun 'shell-command
  "Contains a `shell-command' function.

Override this to your own value for mocking out shell calls in tests.")

(defvar daemons--shell-command-to-string-fun 'shell-command-to-string
  "Contains a `shell-command-to-string' function.
Override this to your own value for mocking out shell calls in tests.")

;; to be defined for each init system
(defvar daemons--commands-alist nil
  "Daemons commands alist.
The car of each pair is the command symbol (e.g. 'stop).
The cdr of each pair is a function taking a daemon name and returning a shell
command to execute.

e.g. '((start . (lambda (x) (format \"service %s start\" x)))
       (stop . (lambda (x) (format \"service %s stop\" x))))")

(defvar daemons--list-fun nil
  "Function to list all daemons.
It should take no arguments and return a list in the right format for
`tabulated-list-entries'.
It will therefore also need to match the columns defined with
`daemons--list-headers-fun'.")

(defvar daemons--list-headers-fun nil
  "Function to get headers for list of all daemons.
It should take no arguments and return a vector in the right format for
`tabulated-list-format'.
It will therefore also need to match the entries returned by `daemons--list-fun'.")

(defvar daemons-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "RET") 'daemons-status-at-point)
    (define-key map (kbd "s") 'daemons-start-at-point)
    (define-key map (kbd "S") 'daemons-stop-at-point)
    (define-key map (kbd "R") 'daemons-restart-at-point)
    (define-key map (kbd "r") 'daemons-reload-at-point)
    map)
  "Keymap for daemons mode.")

(defvar daemons-output-mode-map (copy-keymap daemons-mode-map)
  "Keymap for daemons output mode.")

;; TODO this needs to become buffer-local
(defvar daemons--current-id nil
  "ID of the daemon in the current output buffer.")

;; defuns
(defun daemons--split-lines (string)
  "Split STRING Into list of lines."
  (split-string string "[\n\r]+" t))

(defun daemons--get-list-buffer-name (hostname)
  "Return the buffer name for daemons on HOSTNAME."
  (format "*daemons on %s*" hostname))

(defun daemons--get-output-buffer-name (hostname)
  "Return the buffer name for daemons output on HOSTNAME."
  (format   "*daemons-output on %s*" hostname))

(defun daemons--list ()
  "Return the list of all daemons.

The precise format of the results will depend on the specific subcommand.
It will be different for different init systems, and will match
`daemons--list-headers'."
  (daemons--require-init-system-submodule)
  (funcall daemons--list-fun))

(defun daemons--list-headers ()
  "Return the headers for the list of all daemons.

The results will correspond to the format of each item in `daemons--list'."
  (daemons--require-init-system-submodule)
  (funcall daemons--list-headers-fun))

(defun daemons--shell-command (&rest args)
  "Dynamically bound alias for `shell-command' (to enable test mocks).
ARGS are passed to the underlying function."
  (apply daemons--shell-command-fun args))

(defun daemons--shell-command-to-string (&rest args)
  "Dynamically bound alias for `shell-command-to-string' (to enable test mocks).
ARGS are passed to the underlying function."
  (apply daemons--shell-command-to-string-fun args))

(defun daemons--daemon-at-point ()
  "Return the id of the daemon of the current line if in the list buffer.
Otherwise, return value of ‘daemons--current-id’ variable (set by ‘daemons--run’)."
  (if (derived-mode-p 'tabulated-list-mode)
      (tabulated-list-get-id)
    daemons--current-id))

(defun daemons--insert-header (text)
  "Insert an underlined TEXT header into the buffer."
  (insert (concat (propertize text 'face 'underline) "\n\n")))

(defun daemons--switch-output-buffer-create (hostname)
  "Switch to output buffer for HOSTNAME if it exists, else create it and switch."
  (let ((output-buffer-name (daemons--get-output-buffer-name hostname)))
    (when (not (equal (buffer-name) output-buffer-name))
      (switch-to-buffer-other-window output-buffer-name))))

(defun daemons--run-with-output-buffer (command daemon-name)
  "Run the given COMMAND on DAEMON-NAME.  Show results in an output buffer.

The output buffer is in `daemons-output-mode' and will be switched to if not active."
  (let ((hostname "localhost"))
    (with-current-buffer (get-buffer-create (daemons--get-output-buffer-name hostname))
      (setq buffer-read-only nil
            daemons--current-id daemon-name)
      (when daemons-always-sudo (daemons--sudo))
      (delete-region (point-min) (point-max))
      (daemons--insert-header (format "Output of `%s` on `%s` (%s):" command daemon-name hostname))
      (daemons--run command daemon-name)
      (daemons-output-mode))
    (daemons--switch-output-buffer-create hostname)))

(defun daemons--run (command daemon-name)
  "Run the given COMMAND on DAEMON-NAME.  Insert the results into the current buffer."
  (let ((command-fun (alist-get command daemons--commands-alist)))
    (when (not command-fun)
      (error "No such daemon command: %s" command))
    (daemons--shell-command (funcall command-fun daemon-name) t)))

(defun daemons-guess-init-system-submodule ()
  "Call \"which\" to identify an installed init system."
  (cond ((= 0 (daemons--shell-command "which systemctl")) 'daemons-systemd)
        ((= 0 (daemons--shell-command "which service")) 'daemons-sysvinit)
        ((= 0 (daemons--shell-command "which brew")) 'daemons-brew)
        ((= 0 (daemons--shell-command "which herd")) 'daemons-shepherd)
        (t (error "I'm sorry, your init system isn't supported yet!"))))

(defun daemons--require-init-system-submodule ()
  "Require the appropriate submodule for the init system."
  (require (or daemons-init-system-submodule
                   (daemons-guess-init-system-submodule))))

(defun daemons--sudo ()
  "Become root using TRAMP, but hang out in a temporary directory to minimise damage potential."
  (let ((tempdir (daemons--shell-command-to-string "mktemp -d")))
    (cd (format "/sudo::%s" tempdir))))

(defun daemons--refresh-list ()
  "Refresh the list of daemons."
  (with-current-buffer (get-buffer-create (daemons--get-list-buffer-name "localhost"))
    (setq-local tabulated-list-entries 'daemons--list)))

;; interactive functions
(defun daemons-status-at-point (name)
  "Show the status of the daemon NAME at point in the daemons buffer."
  (interactive (list (daemons--daemon-at-point)))
  (daemons--run-with-output-buffer 'status name))

(defun daemons-start-at-point (name)
  "Start the daemon NAME at point in the daemons buffer."
  (interactive (list (daemons--daemon-at-point)))
  (daemons--run-with-output-buffer 'start name))

(defun daemons-stop-at-point (name)
  "Stop the daemon NAME at point in the daemons buffer."
  (interactive (list (daemons--daemon-at-point)))
  (daemons--run-with-output-buffer 'stop name))

(defun daemons-restart-at-point (name)
  "Restart the daemon NAME at point in the daemons buffer."
  (interactive (list (daemons--daemon-at-point)))
  (daemons--run-with-output-buffer 'restart name))

(defun daemons-reload-at-point (name)
  "Reload the daemon NAME at point in the daemons buffer."
  (interactive (list (daemons--daemon-at-point)))
  (daemons--run-with-output-buffer 'reload name))

(defun daemons--completing-read ()
  "Call `completing-read' with the current daemons list."
  ;; TODO some caching
  (completing-read "Daemon name: " (daemons--list)))

;;;###autoload
(defun daemons-status (name)
  "Show the status of the daemon NAME."
  (interactive (list (daemons--completing-read)))
  (daemons--run-with-output-buffer 'status name))

;;;###autoload
(defun daemons-start (name)
  "Start the daemon NAME.  Show results in an output buffer."
  (interactive (list (daemons--completing-read)))
  (daemons--run-with-output-buffer 'start name))

;;;###autoload
(defun daemons-stop (name)
  "Stop the daemon NAME.  Show results in an output buffer."
  (interactive (list (daemons--completing-read)))
  (daemons--run-with-output-buffer 'stop name))

;;;###autoload
(defun daemons-restart (name)
  "Restart the daemon NAME.  Show results in an ouptut buffer."
  (interactive (list (daemons--completing-read)))
  (daemons--run-with-output-buffer 'restart name))

;;;###autoload
(defun daemons-reload (name)
  "Reload the daemon NAME.  Show results in an output buffer."
  (interactive (list (daemons--completing-read)))
  (daemons--run-with-output-buffer 'reload name))

;;;###autoload
(defun daemons ()
  "Open the list of system daemons (services) for user management.

This opens a ‘daemons-mode’ list buffer.  Move the cursor to a daemon line and
execute one of the commands in `describe-mode' to show status and manage the
state of the daemon."
  (interactive)
  (let ((list-buffer (get-buffer-create (daemons--get-list-buffer-name "localhost"))))
    (with-current-buffer list-buffer
      (display-buffer-pop-up-window list-buffer nil)
      (switch-to-buffer-other-window list-buffer)
      (when daemons-always-sudo (daemons--sudo))
      (daemons--require-init-system-submodule)
      (daemons-mode)
      (daemons--refresh-list)
      (tabulated-list-print t t))))

;; mode definitions
(define-derived-mode daemons-mode tabulated-list-mode
  "Daemons"
  "UI for viewing and controlling system daemons"
  :group 'daemons
  (setq tabulated-list-format (daemons--list-headers)
        tabulated-list-padding 2)
  (add-hook 'tabulated-list-revert-hook 'daemons--refresh-list)
  (tabulated-list-init-header))

;; ;; To demo SysVinit support with mocked-out shell commands:
;; (setq daemons-init-system-submodule 'daemons-sysvinit)
;; (setq daemons--shell-command-to-string-fun (lambda (_) "
;; NetworkManager  0:off   1:off   2:on    3:on    4:on    5:on    6:off
;; abrt-ccpp       0:off   1:off   2:off   3:on    4:off   5:on    6:off
;; abrt-oops       0:off   1:off   2:off   3:on    4:off   5:on    6:off"))
;; (setq daemons--shell-command-fun (lambda (&rest _) (insert "daemon is fucking ded")))

(define-derived-mode daemons-output-mode special-mode
  "Daemons Output"
  "Mode for displaying output of Daemons commands"
  :group 'daemons)

(provide 'daemons)
;;; daemons.el ends here
