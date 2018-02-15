(require 'ert)
(load-file "./services.el")
(load-file "./services-systemd.el")

(ert-deftest systemd-parse-list-test ()
  (let ((input '("accounts-daemon.service                      disabled"
                 "alsa-state.service                           static  "
                 "auditd.service                               enabled "))
        (expected '(("accounts-daemon" ["accounts-daemon" "disabled"])
                    ("alsa-state" ["alsa-state" "static"])
                    ("auditd" ["auditd" "enabled"]))))
    (should (equal expected (services--systemd-parse-list input)))))

(ert-deftest systemd-list-test ()
  (when (file-exists-p "/etc/systemd")
    (let ((expected '("accounts-daemon" ["accounts-daemon" "disabled"]))
          (result (services--systemd-list)))
      (should (seq-contains result expected))
      (should (< 5 (length result))))))
