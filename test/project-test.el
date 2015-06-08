;;; Tests for `tss-project', prefix `tss-project--' is omitted.

(require 'tss-project)
(require 'f)
(require 'dash)
(require 's)

(ert-deftest locate-tsconfig ()
  (let* ((fpath (with-tsconfig-prj-root "ts/animals.ts"))
         (found-root (tss-project/locate-project-root fpath)))
    (should (stringp found-root))
    (should (and (f-exists? found-root) (f-dir? found-root)))
    (should (-any? (lambda (fn)
                     (equal (f-filename fn) tss-project/tsconfig-filename))
                   (f-files found-root)))))

(ert-deftest applicable? ()
  (let* ((file-buf (find-file-noselect (with-tsconfig-prj-root "ts/animals.ts"))))
    (should (tss-client/applicable? tss-project/class file-buf))))

(ert-deftest initialize ()
  (let* ((file-buf (find-file-noselect (with-tsconfig-prj-root "ts/animals.ts")))
         (prjobj (make-instance tss-project/class :buffer file-buf)))
    (tss-client/initialize prjobj)
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
      (should (tss-project/path-within-root? root
                                             "/tmp/some/my.ts"))
      (should-not (tss-project/path-within-root? root
                                                 "/home/my/test.ts")))))
