(load-file "./daemons-systemd.el")

(ert-deftest systemd-parse-list-item-test ()
  (let ((input "accounts-daemon.service                      disabled")
        (expected '("accounts-daemon" ["accounts-daemon" "disabled"])))
    (should (equal expected
                   (daemons-systemd--parse-list-item input)))))

(ert-deftest systemd-is-simple-service-test ()
  (let ((input '("autofs" ["autofs" "disabled"])))
    (should (daemons-systemd--item-is-simple-service-p input)))
  (let ((input '("autovt@" ["autovt@" "disabled"])))
    (should (not (daemons-systemd--item-is-simple-service-p input)))))

(ert-deftest systemd-list-test ()
  (let* ((dummy-output "
accounts-daemon.service                      disabled
alsa-state.service                           static  
auditd.service                               enabled 
autovt@.service                              enabled ")
         (daemons--shell-command-to-string-fun (lambda (_) dummy-output))
         (expected '(("accounts-daemon" ["accounts-daemon" "disabled"])
                     ("alsa-state" ["alsa-state" "static"])
                     ("auditd" ["auditd" "enabled"]))))
    (should (equal expected
                   (daemons-systemd--list)))))

(defconst daemons-systemd-test--failed-status
  (concat
   "\e[0;1;31m×\e[0m dtest-fail.service - failing unit\n"
   "     Loaded: loaded (\e]8;;file://box/u/dtest-fail.service\a/u/dtest-fail.service\e]8;;\a; static)\n"
   "     Active: \e[0;1;31mfailed\e[0m (Result: exit-code) since Sun 2026-10-11 13:17:19 UTC; 1s ago\n"
   "    Process: 4124 ExecStart=/bin/sh -c exit 3 \e[0;1;31m(code=exited, status=3)\e[0m\n"
   "\n"
   "Oct 11 13:17:19 box sh[4124]: \e[0;1;31m\e[0;1;39m\e[0;1;31mboom: something broke\e[0m\n"
   "Oct 11 13:17:19 box sh[4124]: \e[0;1;38;5;185m\e[0;1;39m\e[0;1;38;5;185ma warning\e[0m\n"
   "Oct 11 13:17:19 box sh[4124]: plain info\n"
   "Oct 11 13:17:19 box systemd[1056]: \e[0;1;39m\e[0;1;31m\e[0;1;39mdtest-fail.service: Main process exited\e[0m\n")
  "systemctl status output for a failed unit, as captured with SYSTEMD_COLORS=1.")

(defconst daemons-systemd-test--running-status
  (concat
   "● dtest-ok.service - running unit\n"
   "     Loaded: loaded (/u/dtest-ok.service; static)\n"
   "     Active: active (running) since Sun 2026-10-11 13:17:19 UTC; 1s ago\n"
   "\n"
   "Oct 11 13:17:19 box sh[4122]: started\n")
  "systemctl status output for a running unit.")

(defun daemons-systemd-test--status-buffer (output color)
  "Show OUTPUT as `daemons-status' would, with `daemons-systemd-color' COLOR.
Return the fontified buffer text and the command that was run."
  (let* ((daemons-systemd-color color)
         (daemons-systemd-is-user t)
         (daemons-show-output-in-minibuffer t)
         (daemons-always-sudo nil)
         (default-directory "/tmp/")
         (command nil)
         (daemons--shell-command-fun (lambda (cmd _) (setq command cmd) (insert output))))
    (cl-letf (((symbol-function 'daemons-init-system-submodule) (lambda () 'daemons-systemd))
              ((symbol-function 'message) #'ignore))
      (daemons-status "dtest")
      (with-current-buffer (daemons--get-output-buffer-name
                            (daemons--get-user-and-hostname default-directory))
        (font-lock-ensure)
        (prog1 (list (buffer-string) command)
          (kill-buffer))))))

(defun daemons-systemd-test--face (text string)
  "Return the face on the first character of STRING within TEXT."
  (let ((pos (string-match (regexp-quote string) text)))
    (should pos)
    (or (get-text-property pos 'face text)
        (get-text-property pos 'font-lock-face text))))

(ert-deftest systemd-status-cmd-asks-for-colours-test ()
  (let ((daemons-systemd-is-user t)
        (daemons-systemctl-command-fn #'daemons-systemctl-cmd))
    (let ((daemons-systemd-color t))
      (should (equal "env SYSTEMD_COLORS=1 SYSTEMD_URLIFY=0 systemctl --user status foo"
                     (daemons-systemd--status-cmd "foo"))))
    (let ((daemons-systemd-color nil))
      (should (equal "systemctl --user status foo"
                     (daemons-systemd--status-cmd "foo"))))))

(ert-deftest systemd-status-cmd-colours-reach-systemctl-test ()
  "The colour variables go right before systemctl, inside any sudo wrapper."
  (let ((daemons-systemd-color t))
    (dolist (case '(("sudo systemctl status foo"
                     . "sudo env SYSTEMD_COLORS=1 SYSTEMD_URLIFY=0 systemctl status foo")
                    ("sudo -u bob /usr/bin/systemctl --user status foo"
                     . "sudo -u bob env SYSTEMD_COLORS=1 SYSTEMD_URLIFY=0 /usr/bin/systemctl --user status foo")
                    ("cd /tmp && systemctl status foo"
                     . "cd /tmp && env SYSTEMD_COLORS=1 SYSTEMD_URLIFY=0 systemctl status foo")
                    ("my-status-wrapper foo"
                     . "export SYSTEMD_COLORS=1 SYSTEMD_URLIFY=0; my-status-wrapper foo")
                    ("cd /tmp && my-status-wrapper foo"
                     . "export SYSTEMD_COLORS=1 SYSTEMD_URLIFY=0; cd /tmp && my-status-wrapper foo")
                    ;; A systemctl that is only an argument is left alone.
                    ("test -x /bin/systemctl && systemctl status foo"
                     . "test -x /bin/systemctl && env SYSTEMD_COLORS=1 SYSTEMD_URLIFY=0 systemctl status foo")
                    ("ls /etc/systemctl.d && systemctl status foo"
                     . "ls /etc/systemctl.d && env SYSTEMD_COLORS=1 SYSTEMD_URLIFY=0 systemctl status foo")
                    ("echo systemctl; systemctl status foo"
                     . "echo systemctl; env SYSTEMD_COLORS=1 SYSTEMD_URLIFY=0 systemctl status foo")
                    ("systemctl-wrapper foo"
                     . "export SYSTEMD_COLORS=1 SYSTEMD_URLIFY=0; systemctl-wrapper foo")
                    ("LC_ALL=C sudo systemctl status foo"
                     . "LC_ALL=C sudo env SYSTEMD_COLORS=1 SYSTEMD_URLIFY=0 systemctl status foo")))
      (let ((daemons-systemctl-command-fn (lambda (_command _service) (car case))))
        (should (equal (cdr case) (daemons-systemd--status-cmd "foo")))
        ;; Independent of the syntax table of the buffer it runs in.
        (with-temp-buffer
          (special-mode)
          (should (equal (cdr case) (daemons-systemd--status-cmd "foo"))))))))

(ert-deftest systemd-status-colours-failed-unit-test ()
  (pcase-let ((`(,text ,command)
               (daemons-systemd-test--status-buffer daemons-systemd-test--failed-status t)))
    (should (string-prefix-p "env SYSTEMD_COLORS=1 " command))
    (should-not (string-match-p "\e" text))
    (should (string-match-p "Loaded: loaded (/u/dtest-fail.service; static)" text))
    (should (eq 'daemons-systemd-bad (daemons-systemd-test--face text "failed (Result")))
    (should (eq 'daemons-systemd-label (daemons-systemd-test--face text "Loaded:")))
    (should (eq 'daemons-systemd-label (daemons-systemd-test--face text "Active:")))
    (should (eq 'daemons-systemd-bad (daemons-systemd-test--face text "boom")))
    (should-not (daemons-systemd-test--face text "a warning"))
    (should-not (daemons-systemd-test--face text "plain info"))
    (should-not (daemons-systemd-test--face text "dtest-fail.service: Main"))))

(ert-deftest systemd-status-colours-unit-states-test ()
  (dolist (case '(("active (running)" . daemons-systemd-enabled)
                  ("active (exited)" . daemons-systemd-enabled)
                  ("inactive (dead)" . daemons-systemd-bad)
                  ("activating (start)" . daemons-systemd-changing)
                  ("deactivating (stop-sigterm)" . daemons-systemd-changing)
                  ("reloading (reload)" . daemons-systemd-changing)))
    (let* ((output (replace-regexp-in-string
                    "active (running)" (car case) daemons-systemd-test--running-status t t))
           (text (car (daemons-systemd-test--status-buffer output t))))
      (should (eq (cdr case) (daemons-systemd-test--face text (car case)))))))

(ert-deftest systemd-status-without-colour-test ()
  (pcase-let ((`(,text ,command)
               (daemons-systemd-test--status-buffer daemons-systemd-test--running-status nil)))
    (should (equal "systemctl --user status dtest" command))
    (should-not (daemons-systemd-test--face text "active (running)"))
    (should-not (daemons-systemd-test--face text "Loaded:"))))

(ert-deftest systemd-status-colours-whole-state-test ()
  "The parenthesised part of a failed or changing state is coloured too."
  (dolist (case '(("failed (Result: exit-code)" . daemons-systemd-bad)
                  ("activating (start-pre)" . daemons-systemd-changing)
                  ("maintenance (cleaning)" . daemons-systemd-bad)
                  ("refreshing (mounted)" . daemons-systemd-changing)))
    (with-temp-buffer
      (insert "   Active: " (car case) " since Sun\n")
      (setq font-lock-defaults '(daemons-systemd--status-font-lock-keywords t))
      (font-lock-ensure)
      (goto-char (point-min))
      (search-forward (car case))
      (should (eq (cdr case) (get-text-property (1- (point)) 'face))))))

(ert-deftest systemd-status-colours-bare-active-test ()
  "A unit whose sub-state is its state, such as a target, prints a bare
Active: active, which is coloured too."
  (with-temp-buffer
    (insert "     Active: active since Mon 2026-11-16 09:00:00 CET; 2 days ago\n")
    (setq font-lock-defaults '(daemons-systemd--status-font-lock-keywords t))
    (font-lock-ensure)
    (goto-char (point-min))
    (search-forward "active since")
    (should (eq 'daemons-systemd-enabled
                (get-text-property (- (point) (length " since") 1) 'face)))))

(ert-deftest systemd-setup-output-without-init-system-test ()
  "The output buffer still comes up if the init system cannot be found."
  (let ((daemons-systemd-color t))
    (cl-letf (((symbol-function 'daemons-init-system-submodule)
               (lambda () (error "I'm sorry, your init system isn't supported yet!"))))
      (with-temp-buffer
        (daemons-output-mode)
        (should (derived-mode-p 'daemons-output-mode))))))
