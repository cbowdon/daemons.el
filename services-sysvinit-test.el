(load-file "./services-sysvinit.el")

(ert-deftest sysvinit-parse-list-item-test ()
  (let ((input "NetworkManager  0:off   1:off   2:on    3:on    4:on    5:on    6:off")
        (expected '("NetworkManager" ["0:off" "1:off" "2:on" "3:on" "4:on" "5:on" "6:off"])))
    (should (equal expected
                   (services-sysvinit--parse-list-item input)))))

(ert-deftest sysvinit-list-test ()
  (let* ((dummy-output "
NetworkManager  0:off   1:off   2:on    3:on    4:on    5:on    6:off
abrt-ccpp       0:off   1:off   2:off   3:on    4:off   5:on    6:off
abrt-oops       0:off   1:off   2:off   3:on    4:off   5:on    6:off")
         (services--shell-command-to-string (lambda (_) dummy-output))
         (expected '(("NetworkManager" ["0:off" "1:off" "2:on" "3:on" "4:on" "5:on" "6:off"])
                     ("abrt-ccpp" ["0:off" "1:off" "2:off" "3:on" "4:off" "5:on" "6:off"])
                     ("abrt-oops" ["0:off" "1:off" "2:off" "3:on" "4:off" "5:on" "6:off"]))))
    (should (equal expected
                   (services-sysvinit--list)))))
