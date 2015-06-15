(require 'etss-utils)

(ert-deftest assoc-path:valid-input ()
  (let ((alist '((foo . ((bar . "llama")
                         (baz . "monkey"))))))
    (should (equal (cdr (assoc 'foo alist))
                   (etss-utils/assoc-path alist '(foo))))
    (should (equal (cdr (assoc 'foo alist))
                   (etss-utils/assoc-path alist 'foo)))
    (should (equal "llama"
                   (etss-utils/assoc-path alist '(foo bar))))
    (should (equal "monkey"
                   (etss-utils/assoc-path alist '(foo baz))))
    (should-not (etss-utils/assoc-path alist '(foo nonexistent)))))

(ert-deftest assoc-path:invalid-input ()
  (let ((alist 'some-list))
    (should-not (etss-utils/assoc-path alist 'key))))
