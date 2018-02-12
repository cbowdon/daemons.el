(require 'ert)
(load-file "./services.el")

(ert-deftest split-lines-test ()
  (should (equal '("this" "is" "a" "test") (split-lines "this
is

a


test
"))))

(dolist (test-suite (directory-files "." t "services-.*-test\.el$"))
  (load-file test-suite))

(ert t)
