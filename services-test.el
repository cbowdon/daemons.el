(require 'ert)
(load-file "./services.el")

(ert-deftest split-lines-test ()
  (should (equal '("this" "is" "a" "test") (split-lines "this
is

a


test
"))))

(ert-deftest guess-init-system-submodule-path-test ()
  ;; SysVinit
  (let ((services--shell-command
         (lambda (cmd &rest _) (if (equal cmd "which service") 0 1))))
    (should (equal "./services-sysvinit.el" (services-guess-init-system-submodule-path))))
  ;; systemd
  (let ((services--shell-command
         (lambda (cmd &rest _) (if (equal cmd "which systemctl") 0 1))))
    (should (equal "./services-systemd.el" (services-guess-init-system-submodule-path)))))

(dolist (test-suite (directory-files "." t "services-.*-test\.el$"))
  (load-file test-suite)

  (ert-deftest services--list-headers-test ()
    "Confirm that headers are provided in the correct format."
    (let ((result (services--list-headers)))
      (should (and (vectorp result)
                   (seq-every-p
                    (lambda (el) (equal 3 (length el)))
                    result))))))

(ert t)
