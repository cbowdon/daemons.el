(require 'ert)
(load-file "./services.el")

(ert-deftest split-lines-test ()
  (should (equal '("this" "is" "a" "test") (split-lines "this
is

a


test
"))))

(ert-deftest systemd-list-test ()
  (let ((input '("accounts-daemon.service                      disabled"
                 "alsa-state.service                           static  "
                 "auditd.service                               enabled "))
        (expected '(("accounts-daemon" . (:status "disabled" :original-line "accounts-daemon.service                      disabled"))
                    ("alsa-state" . (:status "static" :original-line "alsa-state.service                           static  "))
                    ("auditd" . (:status "enabled" :original-line "auditd.service                               enabled ")))))
    (should (equal expected (services--systemd-parse-list input)))))

(ert-deftest systemd-list-test ()
  (when (file-exists-p "/etc/systemd")
    (let ((expected '("accounts-daemon" . (:status "disabled" :original-line "accounts-daemon.service                      disabled")))
          (result (services--systemd-list-all)))
      (should (seq-contains result expected))
      (should (< 5 (length result))))))

(ert t)
