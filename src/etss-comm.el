;;; ETSS Communication: code that talks with TS language Service
(require 'etss-client)
(require 'eieio)

(defclass etss-comm/class ()
  ((client :type etss-client/class
           :initarg :client
           ;; TODO there is a bug in `:allow-nil-initform' in inheritance...
           ;; :initform nil
           :documentation "The client use this communication service.")
   ;; TODO we need spec for response format, refer to `tsserver'?
   (response :type (or list vector symbol)
             :initform nil
             :documentation "Last response received in parsed format.")
   (status :type symbol
           :initform :inactive
           :documentation
           "Current status of the communication:
             :active everything is ok.
             :inactive communication is down."))
  :abstract t
  :documentation
  "Abstract base class for all TS Language service, e.g. tss, tsserver and etc.
This class defines the interface between various services and `etss'.")

(defgeneric etss-comm/start ((class etss-comm/class))
  "Start the communication channel with file/project info in CLIENT.")

;;;TODO About sentinels, callbacks and status tracking, we know too little for
;;;now.
;;;#NO-TEST
(defmethod etss-comm/sentinel ((this etss-comm/class))
  "Invoke callbacks in CLIENT.COMM-SENTINELS.

Should be called whenever COMM status changes and before running
observer sentinels, various status info should be set, for now
only STATUS field is defined."
  (with-slots (client) this
    (loop for cb in (oref client comm-sentinels)
          do (funcall cb client))))

(defmethod etss-comm/destroy ((this etss-comm/class))
  "Destroy this communication, close up channels and free up
resources and etc.

Subclasses should call override and call this function in the
last."
  (oset this status :inactive))

(defgeneric etss-comm/command-inspect ((this etss-comm/class) &rest comm-cmds)
  "Send arbitrary command and return :response.
Mainly a development tool.

Subclasses should implement an Emacs interactive command to
display the response.")

;;;: Common Service API
;;;
;;; Define what info language service should supply. Middleware between specific
;;; language service and `etss-client'.
;;;
;;; Each one has specification on parameter list and the format of returned
;;; result.
;;;
(defgeneric etss-comm/alive? ((this etss-comm/class))
  "Check whether this communication channel is still alive.

Usually the method is called by correspondent aliveness test
method in `etss-client/class', so here we should NOT check the
aliveness of `client' (o/w, a loop might occur). ")

(defgeneric etss-comm/update-source ((this etss-comm/class)
                                    source linecount path)
  "Update SOURCE with regards to LINECOUNT, PATH.

Most other `etss-comm' methods relies on updated SOURCE.

Possibly throw errors, return t on success.")

(defgeneric etss-comm/get-completions ((this etss-comm/class)
                                      line column fpath)
  "Retrieve a list of completions at point (LINE COLUMN) in FPATH file.

See `etss-client/get-completions' for more details.")

(defgeneric etss-comm/get-doc ((this etss-comm/class)
                              line column fpath)
  "Retrieve documentation at point (LINE COLUMN) in file FPATH.

See `etss-client/get-doc' for more details.")

(defgeneric etss-comm/get-errors ((this etss-comm/class))
  "Retrieve errors.")

(provide 'etss-comm)
