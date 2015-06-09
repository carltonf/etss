;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;: Client Manager:
;;;
;;; The entry point for `etss' to internal code.
;;; 
;;; Responsibilities:
;;; - Manage all registered client types
;;; - Manage all existing clients and their life cycle, see `etss-client' for detail.
;;; - Identify&Create the right client type of new buffer.
;;; - Responsible for starting/stopping ETSS service.
(eval-when-compile
  (require 'cl))

(require 'etss-client)
(require 'etss-file)
(require 'etss-tsconfig)

(require 'etss-comm)
(require 'etss-tst)

(require 'etss-utils)

(defvar etss--client)

(defvar etss-manager/client-list ()
  "A global list of all `etss-client'.")

;;;#NO-TEST
(defun etss-manager/clean-all-clients ()
  "Helper command to remove all clients, delete all
communications."
  (interactive)
  (let (success-p)
    (unwind-protect
        (progn
          (loop for client in etss-manager/client-list
                do (etss-client/destory client))
          (setq success-p t))
      (unless success-p
        (warn "ETSS: some client-specific cleaning up has failed."))
      (setq etss-manager/client-list nil))))

(defvar etss-manager/registered-client-classes '(etss-tsconfig/class
                                                etss-file/class)
  "A list of registered client class. The order of different
clients are predefined as it affects the type of client will be
used for a buffer.")

;;;#NO-TEST
(defun etss-manager/setup-buffer (file-buf)
  "Main entry for `etss-manager'. Setup ETSS for FILE-BUF."
  (let ((client (etss-manager/client-loaded? file-buf))
        service)
    (unless client
      (let* ((client-class (etss-manager/get-client-class file-buf)))
        (setq client (make-instance client-class :buffer file-buf)
              ;; TODO need options to set what service to use
              service (make-instance etss-tst/class :client client))
        (etss-client/initialize client)
        (etss-client/connect client service)
        (add-to-list 'etss-manager/client-list client)))
    (etss-client/configure-buffer client file-buf)))

(defun etss-manager/client-loaded? (file-buf)
  "Check whether there is an alive client for FILE-BUF. Return
the client if found, o/w nil."
  (loop for client in etss-manager/client-list
        when (etss-client/contains? client file-buf)
        return client))

(defun etss-manager/get-client-class (file-buf)
  "Get the client class that is applicable to FILE-BUF.
See `etss-manager/registered-client-classes' for all possible
classes. Return nil if no class can be used."
  (loop for class in etss-manager/registered-client-classes
        when (etss-client/applicable? class file-buf)
        return class))

;;; TODO better report on which part fails the test
;;;#NO-TEST
(defun etss-manager/aliveness-test (buffer)
  "Check whether ETSS can function normally in BUFFER.

Aliveness test should be done before using ETSS API, this test is
conducted following a chain:
1. global status test at manager level
2. local status, `etss--client' should be set in BUFFER
3. Then `etss--client' -> `etss-comm'."
  ;; global status checking
  (and (and
        etss-manager/client-list
        (etss-client/class-list-p etss-manager/client-list))
       ;; local status checking
       (with-current-buffer buffer
         (and etss--client
              (etss-client/active? etss--client)))))

(provide 'etss-manager)
