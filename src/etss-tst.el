;;; TS Communication for "clausreinke/typescript-tools"
(eval-when-compile
  (require 'cl))
(require 'json)
(require 'pp)

(require 's)
(require 'dash)

(require 'etss-comm)

(defvar etss--client)

(defclass etss-tst/class (etss-comm/class)
  ((proc :type process
         :initform nil
         :documentation "typescript-tools process.")
   (incomplete-response :type string
                        :initform ""
                        :documentation "Incomplete/intermediate ts-tools response, raw JSON string."))
  :allow-nil-initform t
  :documentation
  "TS service class for \"clausreinke/typescript-tools\".")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;: Implementing interface

(defmethod etss-tst/get-start-cmdstr ((this etss-tst/class))
  "Construct a command string to be used by
`start-process-shell-command'."
  (let ((tss-bin (executable-find "tss"))
        cmdstr)
    (unless tss-bin
      (error "tss command not found"))
    (with-slots (name type buffer) (oref this client)
      (pcase type
        (`file
         (setq cmdstr (format "%s %s" tss-bin (buffer-file-name buffer))))
        (`tsconfig
         (setq cmdstr tss-bin))
        (_ (error "Client type [%s] is NOT supported." type))))
    cmdstr))

;;;#NO-TEST
(defmethod etss-comm/start ((this etss-tst/class))
  "Start a ts-tools service for CLIENT."
  (let (client-name
        procnm
        cmdstr
        (process-connection-type nil)
        (waiti 0))
    ;; prepare process
    (with-slots (client status proc response incomplete-response) this
      (setq client-name (oref client name)
            procnm (format "tst-%s" client-name)
            cmdstr (etss-tst/get-start-cmdstr this))
      ;; start a process
      (setq proc (start-process-shell-command procnm nil cmdstr))
      ;; configure the proc
      (when proc
        ;; pass communication object with process property see
        ;; `etss-tst/receive-response-filter'.
        (process-put proc 'comm this)
        (setq response nil
              incomplete-response "")
        (set-process-query-on-exit-flag proc nil)
        (set-process-filter proc #'etss-tst/receive-response-filter)
        (set-process-sentinel proc #'etss-tst/proc-sentinel)

        (etss-tst/accept-response this)

        (when (eq response 'succeed)
          (setq status :active)
          (message "TST: Loaded '%s'." client-name))
        proc))))

;;;#NO-TEST
(defmethod etss-tst/accept-response ((this etss-tst/class) &optional timeout)
  "Accept response from ts-tools.

TIMEOUT is the seconds to wait before return, default to 10 sec.
Return response in the end, which is possibly nil.

The algorithm is busy polling. To ensure the completeness of
response, TIMEOUT is usually quite large, so it is sliced into
multiple short intervals such that at each interval we can check
whether a complete response has been received to return
immediately."
  ;; 0.25 is the trial-and-error magic
  (let* ((slice 0.25)
         (int-count (floor (/ (or timeout 10) slice))))
    (with-slots (proc response) this
      (while (and (> int-count 0)
                  (null response))
        (accept-process-output proc 0.2 nil t)
        (decf int-count))
      response)))

;;;#NO-TEST
(defun etss-tst/proc-sentinel (proc event)
  "ts-tools process sentinel"
  (let ((comm (process-get proc 'comm)))
    (etss-tst/sentinel comm proc event)))

;;;#NO-TEST
(defmethod etss-tst/sentinel ((this etss-tst/class) proc event)
  (message "TST: %s had the event: %s" proc event)
  (let ((pstat (process-status proc)))
    (oset this status (pcase pstat
                        (`run :active)
                        (_ :inactive))))
  (etss-comm/sentinel this))

;;; TODO how to set up a user event system s.t. we can hook things up
;; update mode line
;; TODO: how to set all buffers within a project for now let's do it with a timer
;; (with-current-buffer (window-buffer)
;;   (etss--set-status-mode-line-str))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;: Internal methods

;;;#NO-TEST
(defun etss-tst/receive-response-filter (proc res)
  "Glue function to bridge class methods with process filter functions.

Process property `comm' here is used as a reference for
communication object."
  (let ((comm (process-get proc 'comm)))
    (etss-tst/receive-response comm proc res)))

(defmethod etss-tst/receive-response ((this etss-tst/class) proc rawres)
  "Process filter for ts-tools.

Output from processes can only get processed when Emacs becomes
idle and it's possible to only receive partial result.

Not all output from ts-tools is in JSON format. `tsserver' on the
other hand unifies outputs in standard JSON format."
  (with-slots (response
               incomplete-response) this
    (loop with endre = (rx-to-string `(and bol "\"" (or "loaded" "updated" "added")
                                           (+ space))) ;Note: quoted string within.
          for line in (split-string (or rawres "") "[\r\n]+")
          if (s-equals? line "null")
          return (setq response 'null)
          ;; normal JSON style response
          if (and (s-present? line)
                  (or
                   ;; in the middle of receiving response
                   (s-present? incomplete-response)
                   ;;: start to get response
                   (-some? (lambda (btag) (s-prefix? btag line)) '("{" "["))))
          return (progn
                   (setq incomplete-response (s-concat incomplete-response line)
                         response (etss-tst/parse-response incomplete-response))
                   (when response
                     (setq incomplete-response "")))
          ;; special output: a line of string with special format
          if (string-match endre line)
          return (setq response 'succeed)
          ;; error for server
          if (string-match "\\`\"TSS +\\(.+\\)\"\\'" line)
          do (etss-tst/handle-err-response this line rawres))))

;;; TODO know better about possible error conditions
;;;#+NO-TEST
(defmethod etss-tst/handle-err-response ((this etss-tst/class) line res)
  (warn "TST Handled Errors: [%s] in [%s]" line res)
  ;; (cond ((string= res "closing")
  ;;        nil)
  ;;       ((string-match "\\`command syntax error:" res)
  ;;        nil)
  ;;       (t
  ;;        (tss--debug "Got error response : %s" res)
  ;;        (tss--show-message "%s" res)))
  )

;;;#NO-TEST
(defmethod etss-comm/destroy ((this etss-tst/class))
  (delete-process (oref this proc))
  (call-next-method))

(defconst etss-tst/supported-cmds
  '(;; <cmd> <line> <pos> <file>
    "quickInfo" "definition" "references" "completions" "completions-brief"
    ;; <cmd> <file>/<item>
    "navigationBarItems" "navigateToItems"
    ;; <cmd> (nochech)? <linecount> <file> [linecount lines of source text]
    "update"
    ;; <cmd>
    "reload" "files" "showErrors" "quit")
  "A list of supported commands of typescript-tools, see
https://github.com/clausreinke/typescript-tools for the complete
list and docs.")


;;;#NO-TEST
(defmethod etss-comm/command-inspect ((this etss-tst/class) comm-cmds)
  "Interactive command to send arbitrary command to
typescript-tools.

COMM-CMDS is a list, whose car should be one of
`etss-tst/supported-cmds'."
  (with-slots (response client) this
    (let* ((cbuf (oref client buffer))
           (cmd (car comm-cmds))
           cmdstr
           (posarg (etss-tst/get-posarg cbuf))
           (file (buffer-file-name cbuf)))
      (pcase cmd
        ((or "quickInfo" "definition" "references" "completions" "completions-brief")
         (setq cmdstr (format "%s %s %s" cmd posarg file)))
        ("navigationBarItems"
         (setq cmdstr (format "%s %s" cmd file)))
        ("navigateToItems"
         (let ((item (symbol-at-point)))
           (setq cmdstr (format "%s %s" cmd item))))
        ((or "files" "showErrors")
         (setq cmdstr (format "%s" cmd)))
        ("reload"
         (message "reload currently doesn't respond any meaningful response back."))
        (_
         (error "%s NOT supported yet ;P" cmd)))
      ;;: manual delimiters setup is not used, kept before we document things well.
      ;; (pcase cmd
      ;;   ((or "quickInfo" "definition" "completions" "completions-brief")
      ;;    (setq response-start-tag "{"
      ;;          response-end-tag "}"))
      ;;   ((or "references" "navigationBarItems" "navigateToItems" "files" "showErrors")
      ;;    (setq response-start-tag "["
      ;;          response-end-tag "]"))
      ;;   (_
      ;;    (setq response-start-tag ""
      ;;          response-end-tag "")))

      (etss-tst/send-accept this cmdstr)
      response)))

;;;#NO-TEST
(defmethod etss-tst/send-accept ((this etss-tst/class) msg)
  "Helper method. Issuing most commands require clearing response and this 'send-accept' steps.
Return response.

NOTE: don't use this method on command that don't return output."
  (etss-tst/send-msg this msg)
  (etss-tst/accept-response this))

;;;#NO-TEST
(defmethod etss-tst/send-msg ((this etss-tst/class) msg)
  "Send MSG to typescript tools instance and get the response.

Usually MSG is command string, but it can also be updated source
and etc."
  (with-slots (proc response incomplete-response) this
    (setq response nil
          incomplete-response "")

    ;; TODO error handling
    (process-send-string proc (concat msg "\n"))))

;;;#NO-TEST
(defun etss-tst/cmd-inspect-display (cmd)
  "ELisp interactive command wrapper around
`etss-comm/command-inspect', using `etss-client' local variable."
  (interactive (list
                (progn
                  ;; TODO we need to check the status of `etss-client'
                  (unless etss--client
                    (error "TST: No active etss--client found."))
                  (completing-read "TS Command: "
                                   etss-tst/supported-cmds
                                   nil t))))
  (pp-display-expression (etss-client/comm-inspect etss--client (list cmd))
                         "*TST CMD Inspect*"))

;;;: API implementations
;;;#NO-TEST
(defmethod etss-comm/alive? ((this etss-tst/class))
  (with-slots (client proc status) this
    (and (etss-client/class-child-p client) ;DO NOT CHECK client aliveness!
         (eq status :active)
         ;; process checking (just for safety)
         proc
         (processp proc)
         (eq (process-status proc) 'run))))

;;;#NO-TEST
(defmethod etss-comm/update-source ((this etss-tst/class)
                                    source linecount path)
  ;; Work around a bug about ending blank line. In Emacs, if END is at the
  ;; beginning of a line, `count-lines' won't count the line END is at. However
  ;; ts-tools counts this blank line. A blank string sent would result an error
  ;; (TODO I consider this a bug in ts-tools that should get fixed). So to avoid
  ;; such an error, `incf' linecount.
  (when (s-matches-p "[\n\C-m]\\'" source)
    (incf linecount))

  (let ((cmdstr (format "update %d %s" linecount path)))
    ;; update doesn't output, don't accept
    (etss-tst/send-msg this cmdstr)
    ;; sync source
    (etss-tst/send-accept this source)
    (unless (eq (oref this response) 'succeed)
      (warn "TST: Fail to update source for '%s'." path))))

;;;#NO-TEST
(defmethod etss-comm/get-completions ((this etss-tst/class)
                                      line column fpath)
  ;; in 'tst', "completions" command return a lot extra details
  (let ((cmdstr (format "completions-brief %d %d %s"
                        line (1+ column) fpath)))
    (with-slots (response) this
      (etss-tst/send-accept this cmdstr)
      ;; for completions, `null' means no completions and thus should be nil.
      (when (eq response 'null)
        (setq response nil))
      ;; make sure to return response
      response)))

(defmethod etss-comm/get-doc ((this etss-tst/class)
                              line column fpath)
  (let ((cmdstr (format "quickInfo %d %d %s"
                        line (1+ column) fpath)))
    (etss-tst/send-accept this cmdstr)))

(defmethod etss-comm/get-errors ((this etss-tst/class))
  (let ((cmdstr "showErrors"))
    ;; TODO need to adjust the column value
    (etss-tst/send-accept this cmdstr)))

(defmethod etss-comm/get-definition ((this etss-tst/class)
                                     line column fpath)
  (let* ((cmdstr (format "definition %d %d %s"
                         line (1+ column) fpath))
         (ret (etss-tst/send-accept this cmdstr)))
    ;;TODO adjust column value as Emacs counts column from zero
    ret))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;: Static functions

(defun etss-tst/parse-response (str)
  "Parse STR. If it is complete and well-formated, return parsed
result otherwise nil.

This is NOT validation."
  (ignore-errors
    (json-read-from-string str)))

(defun etss-tst/get-posarg (buffer)
  "Get position argument in the format '<line-num> <col-num>'.

NOTE that Emacs count column from 0, but typescript tools expect
column number from 1."
  (with-current-buffer buffer
    (save-restriction
      (widen)
      (format "%d %d"
              (line-number-at-pos)
              (1+ (- (point) (line-beginning-position)))))))

(provide 'etss-tst)
