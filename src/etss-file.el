;;; ETSS for a single source file
(eval-when-compile
  (require 'cl))

(require 'etss-client)

(defvar etss--client)

(defclass etss-file/class (etss-client/class)
  ((type :type symbol
         :initform 'file))
  :documentation
  "ETSS client for single TypeScript source file.")

;;;: Static Methods
(defmethod etss-client/applicable? :static ((class etss-file/class) file-buf)
  "`etss-file' is always applicable, but this should not be used
before checks for other types of clients."
  t)

;;;: Object Methods
(defmethod etss-client/initialize ((this etss-file/class))
  (with-slots (name buffer initp) this
    (unless (s-present? name)
      (setq name (f-filename (buffer-file-name buffer))))
    ;; after everything has been properly setup
    (setq initp t)))

(defmethod etss-client/contains? ((this etss-file/class) file-buf)
  "Check whether THIS client is for FILE-BUF."
  (s-equals? (buffer-file-name file-buf)
             (buffer-file-name (oref this :buffer))))


;;;#NO-TEST
(defmethod etss-client/active? ((this etss-file/class))
  (with-slots (initp buffer comm) this
    (and initp
         (and (bufferp buffer)
              (buffer-live-p buffer))
         (and (etss-comm/class-child-p comm)
              (etss-comm/alive? comm)))))

;;;#NO-TEST
(defmethod etss-client/destory ((this etss-file/class))
  (with-slots (comm buffer initp) this
    (setq initp nil)
    (etss-comm/destroy comm)
    (with-current-buffer buffer
      (setq etss--client nil))))

(provide 'etss-file)
