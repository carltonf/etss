(require 'tss-file)
(require 's)

(ert-deftest initialize ()
  (let ((client (tss-file/mocker)))
    (tss-client/initialize client)
    (with-slots (initp name) client
      (should (eq initp t))
      (should-not (s-blank? name))))
  (let* ((name "my-name")
         (client (tss-file/mocker :name name)))
    (tss-client/initialize client)
    (should (equal name (oref client name)))))

(ert-deftest contains? ()
  (let ((client (tss-file/mocker)))
    (should (tss-client/contains? client
                                  (find-file-noselect "mockdata/single-file.ts")))
    (should-not (tss-client/contains? client (current-buffer)))))
