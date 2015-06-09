(require 'etss-manager)
(require 'json)

(ert-deftest parse-response ()
  (let ((str "some"))
    (should-not (etss-tst/parse-response str)))
  (let ((str "{\"isMemberCompletion\":false,\"isNewIdentifierLocation\":true,\"entries\":[{\"name\":\"greeter\",\"kind\":\"var\",\"kindModifiers\":\"\",\"type\":\"var greeter: Greeter\",\"docComment\":\"\"}],\"prefix\":\"greeter\"}"))
    (should (etss-tst/parse-response str)))
  (let ((str "{\"isMemberCompletion\":false,\"isNewIdentifierLocation\":true,\"entries\":[{\"name\":\"greeter\",\"kind\":\"var\",\"kindModifiers\":\"\""))
    (should-not (etss-tst/parse-response str)))
  (let ((str "[1, 2, 3]"))
    (should (etss-tst/parse-response str))))


(ert-deftest receive-response:normal ()
  (let ((proc "#<mocked-proc, should NOT be used>")
        (res "mocked message from server, should be set before use.")
        (tst "#<mocked etss-tst/class object, recreate it before each test>"))
    (progn (setq tst (make-instance etss-tst/class)
                 res "\"loaded /home/carl/try/typescript.ts, TSS listening..\"")
           (etss-tst/receive-response tst proc res)
           (should (equal 'succeed
                          (oref tst response))))
    (progn (setq tst (make-instance etss-tst/class)
                 res "null")
           (etss-tst/receive-response tst proc res)
           (should (equal 'null
                          (oref tst response))))
    (progn (setq tst (make-instance etss-tst/class)
                 res "{\"kind\":\"var\",\"kindModifiers\":\"\",\"textSpan\":{\"start\":195,\"length\":7},\"documentation\":[],\"type\":\"var greeter: Greeter\",\"docComment\":\"\"}")
           (etss-tst/receive-response tst proc res)
           (should (equal '((docComment . "")
                            (type . "var greeter: Greeter")
                            (documentation . [])
                            (textSpan
                             (length . 7)
                             (start . 195))
                            (kindModifiers . "")
                            (kind . "var"))
                          (oref tst response))))
    (progn (setq tst (make-instance etss-tst/class)
                 res "[\"/home/carl/.nvm/versions/io.js/v1.6.4/lib/node_modules/typescript-tools/node_modules/typescript/bin/lib.d.ts\",\"/home/carl/try/typescript.ts\"]")
           (etss-tst/receive-response tst proc res)
           (should (equal ["/home/carl/.nvm/versions/io.js/v1.6.4/lib/node_modules/typescript-tools/node_modules/typescript/bin/lib.d.ts" "/home/carl/try/typescript.ts"]
                          (oref tst response))))))

(ert-deftest receive-response:incomplete ()
  (let ((proc "#<mocked-proc, should NOT be used>")
        (res "mocked message from server, should be set before use.")
        (tst "#<mocked etss-tst/class object, recreate it before each test>"))
    (progn (setq tst (make-instance etss-tst/class)
                 res "[\"/home/carl/.nvm/versions/io.js/v1.6.4/lib/node_modules/typescript-tools/node_modules/typescript/bin/")
           (etss-tst/receive-response tst proc res)
           (should (equal nil (oref tst response)))
           (should (equal res (oref tst incomplete-response)))
           (setq res "lib.d.ts\",\"/home/carl/try/typescript.ts\"]")
           (etss-tst/receive-response tst proc res)
           (should (equal ["/home/carl/.nvm/versions/io.js/v1.6.4/lib/node_modules/typescript-tools/node_modules/typescript/bin/lib.d.ts" "/home/carl/try/typescript.ts"]
                          (oref tst response)))
           (should (equal "" (oref tst incomplete-response))))))

(ert-deftest get-start-cmdstr ()
  (let* ((client (etss-file/mocker))
         (tst (make-instance etss-tst/class :client client)))
    (should (string-match-p "^[^ \t]+tss [^ \t]+mockdata.single-file\\.ts$"
                            (etss-tst/get-start-cmdstr tst))))
  (let* ((client (make-instance etss-tsconfig/class))
         (tst (make-instance etss-tst/class :client client)))
    (should (string-match-p "^[^ \t]+tss$" 
                            (etss-tst/get-start-cmdstr tst)))))

(ert-deftest get-posarg ()
  (with-temp-buffer
    (loop for num in (number-sequence 1 100)
          do (insert (format "%4d" num))
          when (zerop (% num 10))
          do (insert "\n"))
    (should (equal "11 1"
                   (etss-tst/get-posarg (current-buffer))))
    (save-excursion
      (search-backward "45")
      (should (equal "5 19"
                     (etss-tst/get-posarg (current-buffer)))))
    (save-excursion
      (goto-char (point-min))
      (should (equal "1 1"
                     (etss-tst/get-posarg (current-buffer)))))))
