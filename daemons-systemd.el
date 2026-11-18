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

(defcustom daemons-systemd-color nil
  "If non-nil, colorize services in systemd buffers according to their statuses.
The status command then runs with SYSTEMD_COLORS=1 SYSTEMD_URLIFY=0, set
by `env' right before systemctl, so that journal lines of priority err
and above can be marked; see `daemons-systemd--status-cmd'.  With a
custom `daemons-systemctl-command-fn' that wraps sudo, sudo then runs
env: if sudoers allows only systemctl, leave this off.  Wrappers that
pass no environment on, such as ssh or sh -c, keep the plain colours of
the state but lose the marks on journal lines."
  :type 'boolean
  :group 'daemons)

(defface daemons-systemd-enabled
  '((((class color)) (:foreground "green")))
  "Face used in systemd buffers for enabled services.")

(defface daemons-systemd-disabled
  '((((class color)) (:foreground "DimGrey")))
  "Face used in systemd buffers for disabled services.")

(defface daemons-systemd-alias
  '((((class color)) (:foreground "cyan")))
  "Face used in systemd buffers for alias services.")

(defface daemons-systemd-linked
  '((((class color)) (:foreground "cyan")))
  "Face used in systemd buffers for linked services.")

(defface daemons-systemd-masked
  '((((class color)) (:foreground "LightCoral")))
  "Face used in systemd buffers for masked services.")

(defface daemons-systemd-static
  '((((class color)) (:foreground "DimGrey" :italic t)))
  "Face used in systemd buffers for static services.")

(defface daemons-systemd-generated
  '((((class color)) (:foreground "silver")))
  "Face used in systemd buffers for generated services.")

(defface daemons-systemd-indirect
  '((((class color)) (:foreground "DimGrey")))
  "Face used in systemd buffers for indirect services.")

(defface daemons-systemd-transient
  '((((class color)) (:foreground "DimGrey")))
  "Face used in systemd buffers for transient services.")

(defface daemons-systemd-bad
  '((((class color)) (:foreground "red" :bold t)))
  "Face used in systemd buffers for bad services.")

(defface daemons-systemd-changing
  '((((class color)) (:foreground "yellow")))
  "Face used in systemd status output for units that are changing state."
  :group 'daemons)

(defface daemons-systemd-label
  '((t (:inherit bold)))
  "Face used in systemd status output for the Loaded: and Active: labels."
  :group 'daemons)

(defun daemons-systemd--color (status string)
  "Colorize STRING according to STATUS."
  (cond ((null daemons-systemd-color) string)
        ((or (string= status "enabled") (string= status "enabled-runtime"))
         (propertize string 'font-lock-face 'daemons-systemd-enabled))
        ((string= status "disabled") (propertize string 'font-lock-face 'daemons-systemd-disabled))
        ((string= status "alias") (propertize string 'font-lock-face 'daemons-systemd-alias))
        ((or (string= status "linked") (string= status "linked-runtime"))
         (propertize string 'font-lock-face 'daemons-systemd-alias))
        ((or (string= status "masked") (string= status "masked-runtime"))
         (propertize string 'font-lock-face 'daemons-systemd-masked))
        ((string= status "static") (propertize string 'font-lock-face 'daemons-systemd-static))
        ((string= status "generated") (propertize string 'font-lock-face 'daemons-systemd-generated))
        ((string= status "indirect") (propertize string 'font-lock-face 'daemons-systemd-indirect))
        ((string= status "transient") (propertize string 'font-lock-face 'daemons-systemd-transient))
        ((string= status "bad") (propertize string 'font-lock-face 'daemons-systemd-bad))
        (t string)))

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

(defconst daemons-systemd--colour-vars "SYSTEMD_COLORS=1 SYSTEMD_URLIFY=0"
  "Environment that makes systemctl colour its output.")

(defconst daemons-systemd--systemctl-command-regexp
  (let ((word "[^ \t;&|()]+"))
    (concat
     ;; The start of a command: of the string, or after ; & | or (.
     "\\(?:\\`\\|[;&|(]\\)[ \t]*"
     ;; Optional variable assignments, such as LC_ALL=C.
     "\\(?:[A-Za-z_][A-Za-z0-9_]*=" "[^ \t;&|()]*" "[ \t]+\\)*"
     ;; An optional sudo or doas with its options and their arguments.
     "\\(?:\\(?:" word "/\\)?\\(?:sudo\\|doas\\)"
     "\\(?:[ \t]+" word "\\)*?[ \t]+\\)?"
     ;; systemctl itself, possibly with a directory, as a whole word.
     "\\(\\(?:" word "/\\)?systemctl\\)\\(?:[ \t]\\|\\'\\)"))
  "Regexp for a systemctl that runs as a command; group 1 is the systemctl.")

(defun daemons-systemd--status-cmd (service)
  "Return the shell command that shows the status of SERVICE.
With `daemons-systemd-color', systemctl is asked for colours so that
`daemons-systemd--mark-journal-errors' can find journal lines of priority
err and above.  The variables are set by `env' right before the first
systemctl that runs as a command, so they survive a wrapper such as sudo,
which drops the caller's environment.  A systemctl elsewhere, such as an
argument of `test -x', is left alone.  A command without such a systemctl
gets them exported first, which keeps shell builtins and compound
commands working."
  (let ((cmd (funcall daemons-systemctl-command-fn "status" service))
        (case-fold-search nil))
    (cond ((not daemons-systemd-color) cmd)
          ((string-match daemons-systemd--systemctl-command-regexp cmd)
           (concat (substring cmd 0 (match-beginning 1))
                   "env " daemons-systemd--colour-vars " "
                   (substring cmd (match-beginning 1))))
          (t (concat "export " daemons-systemd--colour-vars "; " cmd)))))

(defconst daemons-systemd--status-font-lock-keywords
  '(("^ *\\(Loaded\\|Active\\):" 1 'daemons-systemd-label)
    ("^ *Active: \\(active\\(?: ([^)\n]*)\\)?\\)" 1 'daemons-systemd-enabled)
    ("^ *Active: \\(failed\\(?: ([^)\n]*)\\)?\\|inactive (dead)\\)" 1 'daemons-systemd-bad)
    ("^ *Active: \\(maintenance\\(?: ([^)\n]*)\\)?\\)" 1 'daemons-systemd-bad)
    ("^ *Active: \\(\\(?:\\(?:de\\)?activating\\|reloading\\|refreshing\\)\\(?: ([^)\n]*)\\)?\\)"
     1 'daemons-systemd-changing))
  "Font-lock keywords for systemctl status output.")

(defconst daemons-systemd--escape-regexp
  "\e\\[[0-9;:]*m\\|\e]8;;[^\a\e\n]*\\(?:\a\\|\e\\\\\\)"
  "Regexp for the SGR and hyperlink escapes in coloured systemctl output.")

(defun daemons-systemd--mark-journal-errors ()
  "Give journal lines of priority err and above `daemons-systemd-bad'.
systemd starts the message of such a line with bold red; the journal
follows the first empty line after the Active: line.  A bold line with a
red unit name inside, as systemd logs notices, is not one of them."
  (save-excursion
    (goto-char (point-min))
    (when (and (re-search-forward "^ *Active:" nil t)
               (re-search-forward "^$" nil t))
      (while (re-search-forward "^[^\e\n]*: \\(\e\\[0;1;31m\\)" nil t)
        (put-text-property (match-beginning 1) (line-end-position)
                           'font-lock-face 'daemons-systemd-bad)))))

(defun daemons-systemd--strip-escapes ()
  "Delete the escapes coloured systemctl output contains."
  (save-excursion
    (goto-char (point-min))
    (while (re-search-forward daemons-systemd--escape-regexp nil t)
      (replace-match ""))))

(defun daemons-systemd--setup-output ()
  "Colour systemd output in the current `daemons-output-mode' buffer.
Does nothing unless `daemons-systemd-color' is set."
  (when (and daemons-systemd-color
             ;; Not to break the mode if the init system cannot be found.
             (eq (ignore-errors (daemons-init-system-submodule)) 'daemons-systemd))
    (let ((inhibit-read-only t))
      (daemons-systemd--mark-journal-errors)
      (daemons-systemd--strip-escapes))
    (setq-local font-lock-defaults
                '(daemons-systemd--status-font-lock-keywords t))))

(add-hook 'daemons-output-mode-hook #'daemons-systemd--setup-output)

(daemons-define-submodule daemons-systemd
  "Daemons submodule for systemd."

  :test (and (eq system-type 'gnu/linux)
             (executable-find "systemctl"))
  :commands
  '((status . daemons-systemd--status-cmd)
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
         (status (cadr parts)))
    (list name (vector (daemons-systemd--color status name) (daemons-systemd--color status status)))))

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
