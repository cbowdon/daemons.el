(require 'ert)
(load-file "./daemons.el")

(ert-deftest daemons-run-test ()
  (let* (;; mock the shell command function to just echo into buffer
         (daemons--shell-command-fun (lambda (cmd ins) (insert cmd)))
         (expected "COMMAND=foo"))
    ;; mock up a submodule
    (daemons-define-submodule daemons-run-test
      "Test: run submodule"
      :test t
      :commands '((status . (lambda (name) (format "COMMAND=%s" name))))
      :headers [("Daemon name" 100 t)]
      :list '("foo" "bar"))
    ;; add the submodule to the list
    (add-to-list 'daemons-init-system-submodules 'daemons-run-test)
    (should (equal expected
                   (with-temp-buffer
                     (daemons--run 'status "foo")
                     (buffer-string))))))

(ert-deftest daemons-define-submodule-test ()
  "A correct definition should populate `daemons--init-system-submodules-alist'."
  (let ((expected (list :docstring "Test: happy path submodule"
                        :test (lambda () t)
                        :commands nil
                        :list (lambda () nil)
                        :headers (lambda () []))))
    (daemons-define-submodule daemons-happy
      "Test: happy path submodule"
      :test t
      :commands nil
      :headers []
      :list nil)
    (should (equal expected
                   (alist-get 'daemons-happy
                              daemons--init-system-submodules-alist)))))

(ert-deftest daemons--test-submodule-test ()
  (daemons-define-submodule daemons-positive-test
    "Test: positive test submodule"
    :test t
    :commands nil
    :headers []
    :list nil)
  (daemons-define-submodule daemons-negative-test
    "Test: negative test submodule"
    :test nil
    :commands nil
    :headers []
    :list nil)
  (should (daemons--test-submodule 'daemons-positive-test))
  (should (not (daemons--test-submodule 'daemons-negative-test))))

(ert-deftest daemons--commands-alist-test ()
  (let ((expected '((stop . (lambda () "plz stop"))
                    (start . (lambda () "let's go already")))))
    (daemons-define-submodule daemons-commands-test
      "Test: commands submodule"
      :test t
      :commands expected
      :headers []
      :list nil)
    (should (equal expected (daemons--commands-alist 'daemons-commands-test)))))


(ert-deftest daemons--command-test ()
  (let ((expected  (lambda () "let's go already")))
    (daemons-define-submodule daemons-command-test
      "Test: commands submodule"
      :test t
      :commands '((stop . (lambda () "plz stop"))
                    (start . (lambda () "let's go already")))
      :headers []
      :list nil)
    (should (equal expected (daemons--command 'start 'daemons-command-test)))))

(ert-deftest daemons--list-headers-test ()
  (let ((expected [("Daemon name" 80 t)
                   ("Status" 20 t)]))
    (daemons-define-submodule daemons-list-headers-test
      "Test: list headers submodule"
      :test t
      :commands nil
      :headers expected
      :list nil)
    (should (equal expected (daemons--list-headers 'daemons-list-headers-test)))))

(ert-deftest daemons--list-test ()
  (let ((expected '("foo" "bar" "baz")))
    (daemons-define-submodule daemons-list-test
      "Test: list submodule"
      :test t
      :commands nil
      :headers []
      :list expected)
    (should (equal expected (daemons--list 'daemons-list-test)))))

;; system-specific tests
(dolist (test-suite (directory-files "." t "daemons-.*-test\.el$"))
  (load-file test-suite))

;; helper function tests
(ert-deftest daemons--split-lines-test ()
  (should (equal '("this" "is" "a" "test") (daemons--split-lines "this
is

a


test
"))))

(ert-deftest daemons--get-user-and-hostname-test-local ()
  (should (equal (format "%s@%s" (user-login-name) (system-name))
                 (daemons--get-user-and-hostname "/tmp/file"))))

(ert-deftest daemons--get-user-and-hostname-test-remote ()
  (should (equal "me@example.com"
                 (daemons--get-user-and-hostname "/ssh:me@example.com:/etc/issue"))))

(ert t)
