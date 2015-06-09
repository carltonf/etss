;;; Main file of "etss" library
;;;
;;; ETSS is TypeScript language Service for Emacs.
;;;
;;; It strives to serve as a middle layer between various typescript projects
;;; and typescript language service. TSS defines a common set of API that make
;;; 3rd party ELisp packages easily understand ts projects and source code.
;;;
;;; ETSS-IDE is the companion utility package
;;;
;;; Naming Convention:
;;; 1. "etss-" prefix for interactive command or customization options.
;;; 2. "etss--" prefix for ELisp programming API and variables.
(eval-when-compile
  (require 'cl))

(require 'etss-manager)

(defvar-local etss--client nil
  "Reference to the `etss-client/class' for current buffer.")

(defun etss-setup-current-buffer ()
  "Setup ETSS in current buffer"
  (interactive)
  (etss-manager/setup-buffer (current-buffer)))

(defun etss--active? ()
  "Check whether etss has been set up properly in the current
buffer."
  (etss-manager/aliveness-test (current-buffer)))

(defun etss--active-test ()
  "Same as `etss--active?' but if the test fails, throw an error
to prevent any other actions."
  (if (etss--active?)
      t
    (error "ETSS is not properly set up.")))

;;; TODO The format of returned result is not well defined, as we need to know
;;; more about typescript spec to finalize them. For now the format is basically
;;; the one returned by the `typescript-tools'.

(defun etss--get-completions ()
  "Get a list of completions at current point in the current
buffer. nil if none is found.

  Example of returned result:
  ((entries .
            [((kindModifiers . <|public|private|...>)
              (kind . <property|...>)
              (name . <name>))
             |...])
   (isNewIdentifierLocation . :json-false)
   (isMemberCompletion . t))

See `etss-client/get-completions' for details."
  (etss--active-test)
  (let ((client etss--client)
        (cbuf (current-buffer)))
    (etss-client/set-buffer client cbuf)
    (etss-client/sync-buffer-content client)
    (etss-client/get-completions client)))

(defun etss--get-doc-at-point ()
  "Get documentation on thing at point.

Example of returned result:
  ((docComment . <string>)
   (type . <full string of declaration>)
   (documentation . <array of something?>)
   (textSpan
    (length . 17)
    (start . 216))
   (kindModifiers . <declare|...>)
   (kind . <kind>))

See `etss-client/get-doc'"
  (etss--active-test)
  (let ((client etss--client)
        (cbuf (current-buffer)))
    (etss-client/set-buffer client cbuf)
    (etss-client/sync-buffer-content client)
    (etss-client/get-doc client)))

(defun etss--get-errors ()
  "Get a list of errors on `etss--client'.

NOTE this is not only errors for current buffer.

Example of returned result:
  [((category . \"Error\")
    (phase . \"Semantics\")
    (code . 2322)
    (text . \"Type 'string' is not assignable to type 'number'.\")
    (end
     (character . 10)
     (line . 1))
    (start
     (character . 5)
     (line . 1))
    (file . <file-path>))
   ((category . \"Error\")
    (phase . \"Semantics\")
    (code . 2322)
    (text . \"Type 'number' is not assignable to type 'string'.\")
    (end
     (character . 10)
     (line . 12))
    (start
     (character . 5)
     (line . 12))
    (file . <file-path>))]. "
  (etss--active-test)
  (let ((client etss--client)
        (cbuf (current-buffer)))
    (etss-client/set-buffer client cbuf)
    (etss-client/sync-buffer-content client)
    (etss-client/get-errors client)))

(defun etss--request-errors-bg ()
  "Request errors info in the background. Unlike
`etss--get-errors', this function returns immediately. Use
`etss--requested-errors' to retrieve the result.

Getting errors seem to be time consuming, so an async function is
useful if the client should not block."
  (etss--active-test)
  (error "Not implemented."))

(defun etss--requested-errors ()
  "Return the errors requested beforehand with
`etss--request-errors-bg'. If error info is ready, return it.
Otherwise return nil. To guarantee getting results, the client
code can do the busy polling."
  (etss--active-test)
  (error "Not implemented."))

(provide 'etss)
