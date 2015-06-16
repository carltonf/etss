;;;: Generic ETSS Client
;;;
;;; Abstract internal representation of ETSS client.
;;; Various types of project and file are all subclass of TSS client
;;;
;;; Life Cycle of a ETSS client, managed by `etss-manager'
;;; - Visiting a new file can lead to creation of a new client
;;; - All "active" clients are enlisted in `etss-manager/client-list'.
;;; - When `etss-client/active?' return false, `etss-client/destroy' should be
;;;   called to clean up.
;;; - Upon fatal errors or user request, `etss-manager' can destroy a client.

(eval-when-compile
  (require 'cl))
(require 'eieio)

(require 'etss-comm)
(require 'etss-utils)

(defvar etss--client)

(defclass etss-client/class ()
  ((name :initarg :name
         :initform ""
         :type string
         :documentation "Client name. If not set, a name will be derived from `buffer'.")
   (buffer :initarg :buffer
           :type buffer
           :documentation "`current-buffer' of the client. Need
           to be updated for methods that need to access buffer
           info.")
   (comm :type etss-comm/class
         :documentation "Current ETSS communication object for the client.")
   (comm-sentinels :type list
                   :initform nil
                   :documentation
                   "A list of callback functions that get called
                   when COMM status changed. Callbacks will be
                   passed with THIS object.")
   (type :type symbol
         :initform nil
         :documentation "TS file/project types, currently only 'file and 'tsconfig.")
   ;; internal status for life cycle
   (initp :type boolean
          :initform nil
          :documentation "Set by constructor to indicate a properly initialized object."))
  :abstract t
  :documentation
  "Abstract base class for all ETSS clients, e.g. files, various
  project types and etc. This is the interface `etss-manager' will
  see.")

;;;: Static Methods
(defgeneric etss-client/applicable? ((class etss-client/class) file-buf)
  "A STATIC method. Check whether client type CLASS is applicable
to FILE-BUF. In case of a project, this is just whether the file
is contained by this project.")

;;;: Object Methods
(defgeneric etss-client/initialize ((this etss-client/class))
  "Initialize objects of `etss-client/class', should be called
before any use of the objects.")

(defgeneric etss-client/contains? ((this etss-client/class) file-buf)
  "Check whether THIS client contains FILE-BUF.")

(defmethod etss-client/connect ((this etss-client/class) service)
  "Connect to TS language Service."
  (with-slots (comm) this
    (setq comm service)
    (etss-comm/start comm)))

(defgeneric etss-client/active? ((this etss-client/class))
  "Check whether THIS client is still active. If not, usually a
`etss-client/destory' call is followed.")

(defgeneric etss-client/destory ((this etss-client/class))
  "Destroy THIS, clean up and free resources. In particular,
configurations done to buffer in `etss-client/configure-buffer'
should be undone.")

;;;#NO-TEST
(defmethod etss-client/configure-buffer ((this etss-client/class) buffer)
  "Configure BUFFER with regards to THIS client.

If subclasses override this function and they should call this
function in the last."
  (with-current-buffer buffer
    (setq etss--client this)))

;;;#NO-TEST
(defmethod etss-client/comm-inspect ((this etss-client/class) comm-cmds)
  "Inspect communication for THIS client. COMM-CMDS is a list and
can be anything that the underlying communication understands.
This method is internal and serves development purpose mostly.

NOTE: Communication inspection is communication specific, so the
entry interactive command is defined per communication type.
However no communication can be conducted without some preps or
info supplied by `etss-client/class', so we internally need this
method `etss-client/comm-inspect' as the inspection entry which
later delegates real work to specific communication methods.

As this is a development tool, abstraction layer is not well
contained, e.g. `buffer' is usually not used by `etss-comm' but
here `etss-comm/command-inspect' will extract info directly from
buffer."
  (etss-client/set-buffer this)
  (etss-client/sync-buffer-content this)
  (etss-comm/command-inspect (oref this comm) comm-cmds))

;;;: Set of supported etss-client API
;;;
;;; API for 3rd ELisp library to utilize ETSS. See notes below for usage
;;; cautions.
;;;
;;; A normal flow of using the APIs:
;;; 0. use `etss--active?' which calls `etss-client/active?' and some others
;;; 1. `etss-client/set-buffer' set up buffer.
;;; 2. `etss-client/sync-buffer-content' sync/update source.
;;; 3. Prepare needed params, usually the default is sufficient.
;;; 4. Call specific `etss-client' API.
;;; 5. Retrieve response, extract info. Handle errors if any.
;;;
;;;
;;; Usually 3rd party should use more API wrappers offered in `etss.el'. Only
;;; when they need extra flexibility, the `etss-client' API is used.
;;;
;;; Notes on the two API sets: `etss.el' and `etss-client.el'
;;; 1. `etss.el' expose the conventional ELisp style API: no need to pass EIEIO object.
;;; 2. `etss.el' only deal with the most common use cases: the current buffer,
;;; the current point and etc. AND there is no need to go through the "normal"
;;; flow documented above.
;;; 3. `etss-client.el' tries to attain maximum flexibility.
;;; 4. `etss.el' is considered more stable than `etss-client.el', as the latter
;;; needs to adjust for new use cases more often
;;;
;;;
;;;

;;;#NO-TEST
(defmethod etss-client/set-buffer ((this etss-client/class) &optional buffer)
  "Update :buffer to BUFFER or `current-buffer' if BUFFER is nil.
Needed as various commands can only be done with regards to the
`current-buffer'."
  (let ((buf (or buffer (current-buffer))) )
    (when (etss-client/contains? this buf)
      (oset this :buffer buf))))

;;; TODO non-optimized and not well-scaled.
;;; 1. Projects have multiple buffers not a single buffer
;;; 2. Use timestamps to only sync updated buffers.
;;; 3. Make the return value indicate whether a sync has happened, some methods
;;; are time-consuming and should not run if no content updates.
;;; 4. Change the name and delegate the work to client.
;;;#NO-TEST
(defmethod etss-client/sync-buffer-content ((this etss-client/class)
                                            &optional source linecount)
  "Sync buffer content with ts service. By default file path, content,
line count are retrieved from `current-buffer'.

Optional arguments SOURCE, LINECOUNT are supplied to allow extra
flexibility in updating the source. These are useful when you are
doing completing/templating when the needed changes are not even
in buffer yet, but you still want to get some info about these
supposed changes (like definition/quickInfo and etc.).

Syncing content is `etss-client/class's responsibility. If needed,
client should call this method before issuing commands to `comm'.

TODO I think these are needed because the ts service only support
stateless queries."
  (with-current-buffer (oref this buffer)
    (save-restriction
      (widen)
      (let* ((source (or source
                         (buffer-substring-no-properties (point-min) (point-max))))
             (linecount (or linecount
                            (count-lines (point-min) (point-max)))))
        (etss-comm/update-source (oref this comm) source linecount (buffer-file-name))))))

(defmethod etss-client/get-completions ((this etss-client/class)
                                        &optional line column)
  "Get completions at point (LINE COLUMN) or current point. A
list of completions should only contain basic info, there should
be NO detailed info."
  (with-current-buffer (oref this :buffer)
    (save-restriction
      (widen)
      (let ((line (or line (line-number-at-pos)))
            (column (or column (current-column))))
        (etss-comm/get-completions (oref this comm)
                                   line column (buffer-file-name))))))

(defmethod etss-client/get-doc ((this etss-client/class)
                                &optional line column)
  "Get documentation on thing at (LINE COLUMN) or current point."
  (with-current-buffer (oref this :buffer)
    (save-restriction
      (widen)
      (let ((line (or line (line-number-at-pos)))
            (column (or column (current-column))))
        (etss-comm/get-doc (oref this comm)
                           line column (buffer-file-name))))))

;;; TODO incomplete, need error caching, actually we need general API caching to
;;; optimize and implement async functionality.
(defmethod etss-client/get-errors ((this etss-client/class))
  "Retrieve errors about THIS client."
  (etss-comm/get-errors (oref this comm)))


;;;#NO-TEST
(defmethod etss-client/get-definition ((this etss-client/class)
                                       &optional line column)
  "Get definition description for thing at (LINE COLUMN) or current point.

Return nil if not found or "
  (with-current-buffer (oref this :buffer)
    (save-restriction
      (widen)
      (let* ((line (or line (line-number-at-pos)))
             (column (or column (current-column)))
             (ret (etss-comm/get-definition (oref this comm)
                                            line column (buffer-file-name)))
             (deffile (etss-utils/assoc-path ret 'file))
             (line (etss-utils/assoc-path ret '(min line)))
             (col (etss-utils/assoc-path ret '(min character))))
        ;; sanity check
        (if (or (not deffile)
                (not (file-exists-p deffile))
                (null line)
                (null col))
            nil
          ret)))))
(provide 'etss-client)
