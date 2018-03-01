(require 'ert)
(load-file "./daemons.el")

(ert-deftest split-lines-test ()
  (should (equal '("this" "is" "a" "test") (daemons--split-lines "this
is

a


test
"))))

(ert-deftest run-command-test ()
  (let* (;; dummy daemons module definition
         (daemons--commands-alist '((status . (lambda (name) (format "COMMAND=%s" name)))))
         (daemons--list-fun (lambda () '("foo" "bar")))
         (daemons--list-headers-fun (lambda () [("Daemon name" 100 t)]))
         ;; mock the shell command function to just echo into buffer
         (daemons--shell-command-fun (lambda (cmd ins) (insert cmd)))
         (expected "COMMAND=foo"))
    (should (equal expected
                   (with-temp-buffer
                     (daemons--run 'status "foo")
                     (buffer-string))))))

(ert-deftest guess-init-system-submodule-test ()
  ;; SysVinit
  (let ((daemons--shell-command-fun
         (lambda (cmd &rest _) (if (equal cmd "which service") 0 1))))
    (should (equal 'daemons-sysvinit (daemons-guess-init-system-submodule))))
  ;; systemd
  (let ((daemons--shell-command-fun
         (lambda (cmd &rest _) (if (equal cmd "which systemctl") 0 1))))
    (should (equal 'daemons-systemd (daemons-guess-init-system-submodule)))))

;; system-specific tests
(dolist (test-suite (directory-files "." t "daemons-.*-test\.el$"))
  (load-file test-suite)

  (ert-deftest daemons--list-headers-test ()
    "Confirm that headers are provided in the correct format."
    (let ((result (daemons--list-headers)))
      (should (and (vectorp result)
                   (seq-every-p
                    (lambda (el) (equal 3 (length el)))
                    result))))))

(ert t)
