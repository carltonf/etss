;;; Tests for `etss-tsconfig', prefix `etss-tsconfig--' is omitted.

(require 'etss-tsconfig)
(require 'f)
(require 'dash)
(require 's)

(ert-deftest locate-tsconfig ()
  (let* ((fpath (with-tsconfig-prj-root "ts/animals.ts"))
         (found-root (etss-tsconfig/locate-project-root fpath)))
    (should (stringp found-root))
    (should (and (f-exists? found-root) (f-dir? found-root)))
    (should (-any? (lambda (fn)
                     (equal (f-filename fn) etss-tsconfig/tsconfig-filename))
                   (f-files found-root)))))

(ert-deftest applicable? ()
  (let* ((file-buf (find-file-noselect (with-tsconfig-prj-root "ts/animals.ts"))))
    (should (etss-client/applicable? etss-tsconfig/class file-buf))))

(ert-deftest initialize ()
  (let* ((file-buf (find-file-noselect (with-tsconfig-prj-root "ts/animals.ts")))
         (prjobj (make-instance etss-tsconfig/class :buffer file-buf)))
    (etss-client/initialize prjobj)
    (with-slots (name buffer buflist root initp) prjobj
      (should (s-equals? name "tsconfig-prj"))
      (should (eq buffer file-buf))
      (should (and (eq (length buflist) 1)
                   (eq buffer (car buflist))))
      (should (and (f-dir? root)
                   (s-equals? (f-filename root) "tsconfig-prj")))
      (should (eq initp t)))))

(ert-deftest path-within-root? ()
  (with-temp-buffer
    (let ((root "/tmp/"))
      (should (etss-tsconfig/path-within-root? root
                                             "/tmp/some/my.ts"))
      (should-not (etss-tsconfig/path-within-root? root
                                                 "/home/my/test.ts")))))
