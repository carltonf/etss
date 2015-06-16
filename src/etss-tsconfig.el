;;; TS Project with a "tsconfig.json" file
;;;
;;; Only projects as specified by the upstream are supported.
;;;
;;; TODO
;;; - [ ] Integrated with `projectile', which is a popular package supports
;;;   a lot project-wide facilities.
(eval-when-compile
  (require 'cl))

(require 'etss-client)

(defvar etss--client)

(defconst etss-tsconfig/tsconfig-filename "tsconfig.json"
  "TS project configuration file, see
https://github.com/Microsoft/TypeScript/wiki/tsconfig.json")

(defclass etss-tsconfig/class (etss-client/class)
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
  :documentation "ETSS client class for tsconfig project.")

(defun etss-tsconfig/locate-project-root (fpath)
  "Try to locate a the project root for FPATH. This function
searches upwards to find the nearest ancestor containing
`etss-tsconfig/tsconfig-filename'."
  (locate-dominating-file fpath etss-tsconfig/tsconfig-filename))

(defmethod etss-client/applicable? :static ((class etss-tsconfig/class) file-buf)
  "Check whether a FILE-BUF belongs to a
  `etss-tsconfig/tsconfig-filename' project."
  (etss-tsconfig/locate-project-root (buffer-file-name file-buf)))

(defmethod etss-client/initialize ((this etss-tsconfig/class))
  (with-slots (name buffer buflist root initp) this
    (let ((fpath (buffer-file-name buffer)))
      (setq root (etss-tsconfig/locate-project-root fpath))
      (unless (s-present? name)
        (setq name (f-filename root)))
      (add-to-list 'buflist buffer))
    ;; after everything has been properly setup
    (setq initp t)))

;;;NO-TEST
(defmethod etss-client/contains? ((this etss-tsconfig/class) file-buf)
  "Whether FILE-BUF belongs to THIS project."
  (let ((fpath (buffer-file-name file-buf)))
    (with-slots (root) this
      (etss-tsconfig/path-within-root? root fpath))))

(defsubst etss-tsconfig/path-within-root? (root fpath)
  "Pure function to test whether FPATH is under ROOT."
  (s-prefix? (expand-file-name root) (expand-file-name fpath)))

(defmethod etss-client/active? ((this etss-tsconfig/class))
  (with-slots (initp comm) this
    (and initp
         (and (etss-comm/class-child-p comm)
              (etss-comm/alive? comm)))))

(defmethod etss-client/destory ((this etss-tsconfig/class))
  (with-slots (comm buflist initp) this
    (setq initp nil)
    (etss-comm/destroy comm)
    (loop for buf in buflist
          do (with-current-buffer buf
               (setq etss--client nil)))))

(provide 'etss-tsconfig)
