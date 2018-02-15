(require 'ert)
(load-file "./services.el")

(ert-deftest split-lines-test ()
  (should (equal '("this" "is" "a" "test") (split-lines "this
is

a


test
"))))

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
