(eval-when-compile
  (require 'cl))

;;; NOTE the CWD set by `cask' is where CASK file located
(add-to-list 'load-path (expand-file-name "src"))

(defun* tss-file/mocker (&key name)
  "Mock a `tss-file' object for testing."
  (let* ((buffer (find-file-noselect "mockdata/single-file.ts"))
         (client (make-instance tss-file/class
                                :buffer buffer
                                :name (or name ""))))
    client))

(defun* tss-manager/client-list-mocker ()
  "Mock a `tss-manager/client-list', use `let' binding to use."
  (let ((clist (list (tss-file/mocker))))
    clist))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; For `tsconfig' project
(defmacro with-tsconfig-prj-root (subpath)
  "Return a complete path to SUBPATH under tsconfig project mockdata."
  `(f-join "test/mockdata/tsconfig-prj" ,subpath))
