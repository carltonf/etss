;;; TS Project with a "tsconfig.json" file
;;;
;;; Only projects as specified by the upstream are supported.
;;;
;;; TODO
;;; - Rename this project to `tss-tsconfig'
;;; - [ ] Support arbitrary project. This requires a way to pass arbitrary parameters
;;; (including tsc parameters and files) to TSS.
;;; - [ ] Integrated with `projectile', which is a popular package supports a lot project-wide
;;; facilities.

(require 'tss-client)

(defconst tss-project/tsconfig-filename "tsconfig.json"
  "TS project configuration file, see
https://github.com/Microsoft/TypeScript/wiki/tsconfig.json")

(defclass tss-project/class (tss-client/class)
  ((type :type symbol
         :initform 'tsconfig)
   (root :type string
         :initform ""
         :documentation "Project root.")
   (buflist :type list
            :initform nil
            ;; TODO add management code
            :documentation "A list of live buffers currently
            belonging to this client."))
  :documentation "TSS client class for tsconfig project.")

(defun tss-project/locate-project-root (fpath)
  "Try to locate a the project root for FPATH. This function
searches upwards to find the nearest ancestor containing
`tss-project/tsconfig-filename'."
  (locate-dominating-file fpath tss-project/tsconfig-filename))

(defmethod tss-client/applicable? :static ((class tss-project/class) file-buf)
  "Check whether a FILE-BUF belongs to a
  `tss-project/tsconfig-filename' project."
  (tss-project/locate-project-root (buffer-file-name file-buf)))

(defmethod tss-client/initialize ((this tss-project/class))
  (with-slots (name buffer buflist root initp) this
    (let ((fpath (buffer-file-name buffer)))
      (setq root (tss-project/locate-project-root fpath))
      (unless (s-present? name)
        (setq name (f-filename root)))
      (add-to-list 'buflist buffer))
    ;; after everything has been properly setup
    (setq initp t)))

;;;NO-TEST
(defmethod tss-client/contains? ((this tss-project/class) file-buf)
  "Whether FILE-BUF belongs to THIS project."
  (let ((fpath (buffer-file-name file-buf)))
    (with-slots (root) this
      (tss-project/path-within-root? root fpath))))

(defun tss-project/path-within-root? (root fpath)
  "Pure function to test whether FPATH is under ROOT."
  (s-prefix? root fpath))

(defmethod tss-client/destory ((this tss-project/class))
  (with-slots (comm buflist initp) this
    (setq initp nil)
    (tss-comm/destroy comm)
    (loop for buf in buflist
          do (with-current-buffer buf
               (setq tss--client nil)))))

(provide 'tss-project)
