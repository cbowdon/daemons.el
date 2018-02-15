(load-file "./services-systemd.el")

(ert-deftest systemd-parse-list-item-test ()
  (let ((input "accounts-daemon.service                      disabled")
        (expected '("accounts-daemon" ["accounts-daemon" "disabled"])))
    (should (equal expected
                   (services-systemd--parse-list-item input)))))

(ert-deftest systemd-list-test ()
  (let* ((dummy-output "
accounts-daemon.service                      disabled
alsa-state.service                           static  
auditd.service                               enabled ")
         (services--shell-command-to-string-fun (lambda (_) dummy-output))
         (expected '(("accounts-daemon" ["accounts-daemon" "disabled"])
                     ("alsa-state" ["alsa-state" "static"])
                     ("auditd" ["auditd" "enabled"]))))
    (should (equal expected
                   (services-systemd--list)))))
