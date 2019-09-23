(load-file "./daemons-openrc.el")

(ert-deftest openrc-parse-list-item-test ()
  (let ((input "   dummy |  boot    default    nonetwork   shutdown    sysinit  ")
        (expected '("dummy" ["dummy" "boot" "default" "nonetwork" "shutdown" "sysinit"])))
    (should (equal expected
                   (daemons-openrc--parse-list-item input)))))

(ert-deftest openrc-list-test ()
  (let* ((dummy-output "
dummy1 |  boot    
dummy2 |          default
dummy3 |                    nonetwork     
dummy4 |                                shutdown  
dummy5 |                                           sysinit ")
         (daemons--shell-command-to-string-fun (lambda (_) dummy-output))
         (expected '(("dummy1" ["dummy1" "boot" "" "" "" ""])
		     ("dummy2" ["dummy2" "" "default" "" "" ""])
		     ("dummy3" ["dummy3" "" "" "nonetwork" "" ""])
		     ("dummy4" ["dummy4" "" "" "" "shutdown" ""])
		     ("dummy5" ["dummy5" "" "" "" "" "sysinit"]))))
    (should (equal expected
                   (daemons-openrc--list)))))
