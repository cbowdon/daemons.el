(load-file "./services-sysvinit.el")

(ert-deftest sysvinit-parse-list-test ()
  (let ((input "NetworkManager  0:off   1:off   2:on    3:on    4:on    5:on    6:off")
        (expected '("NetworkManager" ["0:off" "1:off" "2:on" "3:on" "4:on" "5:on" "6:off"])))
    (should (equal expected
                   (services-sysvinit--parse-list-item input)))))
