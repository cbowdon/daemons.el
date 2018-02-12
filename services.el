;; -*- lexical-binding: t -*-
(require 'seq)

;; declarations
(defconst services--dashboard-buffer-name "*services*")
(defconst services--output-buffer-name "*services-output*")
(defconst services--error-buffer-name "*services-error*")

(defvar services--commands-alist nil "Services commands alist")
(defvar services--commands-alist-systemd nil "Services commands alist for systemd")

(defvar services-mode-map nil "Keymap for services mode")

(defvar services-list nil "List of current system services")

;; assignments
(setq services-mode-map
      (let ((map (make-sparse-keymap)))
        (define-key map [tab] 'services-next-line)
        (define-key map [backtab] 'services-prev-line)
        (define-key map (kbd "g") 'services-refresh-dashboard)
        (define-key map (kbd "n") 'services-next-line)
        (define-key map (kbd "p") 'services-prev-line)
        (define-key map (kbd "RET") 'services-status-current)
        (define-key map (kbd "s") 'services-start-current)
        (define-key map (kbd "S") 'services-stop-current)
        map))

(setq services--commands-alist-systemd
      '((list . services--systemd-list-all)
        (pretty-print . services--systemd-pretty-print)
        (show . (lambda (name) (format "systemctl show %s" name)))
        (status . (lambda (name) (format "systemctl status %s" name)))
        (start . (lambda (name) (format "systemctl start %s" name)))
        (stop . (lambda (name) (format "systemctl stop %s" name)))
        (restart . (lambda (name) (format "systemctl restart %s" name)))))

(setq services--commands-alist services--commands-alist-systemd)

;; defuns
(defun split-lines (string)
  "Split STRING Into list of lines"
  (split-string string "[\n\r]+" t))

(defun services--systemd-parse-list (services-list)
  (seq-map
   (lambda (line)
     (let ((parts (split-string line)))
       (cons
        (replace-regexp-in-string "\.service" "" (car parts))
        (list :status (cadr parts)
              :original-line line))))
   services-list))

(defun services--systemd-list-all ()
  "Return an alist of services on a systemd system.
  The car of each cons pair is the service name.
  The cdr is a plist of extended properties (e.g. enabled/disabled status)."
  (thread-last  "systemctl list-unit-files --type=service --no-legend"
    (shell-command-to-string)
    (split-lines)
    (services--systemd-parse-list)))

(defun services--systemd-pretty-print (service)
  "Produce a formatted string describing a service"
  (let ((name (car service))
        (props (cdr service)))
    (format "%s\t\t[%s]\n" name (plist-get props :status))))

(defun services--list-all ()
  (funcall (alist-get 'list services--commands-alist)))

(defun services--pretty-print (service)
  (funcall (alist-get 'pretty-print services--commands-alist) service))

(defun services-next-line ()
  "Move the cursor the next line"
  (interactive)
  (beginning-of-line 2))

(defun services-prev-line ()
  "Move the cursor the prev line"
  (interactive)
  (beginning-of-line 0))

(defun services--current ()
  (let ((index (- (line-number-at-pos) (services--header-lines) 1)))
    (nth index services-list)))

(defun services-show-current ()
  (interactive)
  (services--run 'show))

(defun services-status-current ()
  (interactive)
  (services--run 'status))

(defun services-start-current ()
  (interactive)
  (services--run 'start))

(defun services-stop-current ()
  (interactive)
  (services--run 'stop))

(defun services--run (command)
  (let ((service-name (car (services--current)))
        (command-fun (alist-get command services--commands-alist)))
    (async-shell-command (funcall command-fun service-name)
                         (get-buffer-create services--output-buffer-name)
                         (get-buffer-create services--error-buffer-name))))

;; mode definition
(define-derived-mode services-mode special-mode
  "Services"
  "Dashboard for viewing and controlling system services"
  (linum-mode -1)
  (page-break-lines-mode 1)
  (whitespace-mode -1)
  (setq buffer-read-only nil
        truncate-lines t))

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
