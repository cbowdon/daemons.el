(load-file "./daemons-shepherd.el")

(ert-deftest shepherd-parse-list-item-test ()
  (let ((input "+ compton")
        (expected '("compton" ["compton" "started"])))
    (should (equal expected
                   (daemons-shepherd--parse-list-item input)))))

(ert-deftest shepherd-is-service-line-test ()
  (let ((input " - redshift"))
    (should (daemons-shepherd--item-is-service-p input)))
  (let ((input " + gpg-agent"))
    (should (daemons-shepherd--item-is-service-p input)))
  (let ((input "Started:"))
    (should (not (daemons-shepherd--item-is-service-p input))))
  (let ((input "Stopped:"))
    (should (not (daemons-shepherd--item-is-service-p input)))))

(ert-deftest shepherd-list-test ()
  (let* ((dummy-output "
Started:
 + compton
 + root
Stopped:
 - gpg-agent")
         (daemons--shell-command-to-string-fun (lambda (_) dummy-output))
         (expected '(("compton" ["compton" "started"])
                     ("root" ["root" "started"])
                     ("gpg-agent" ["gpg-agent" "stopped"]))))
    (should (equal expected
                   (daemons-shepherd--list)))))
