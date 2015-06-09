(require 'etss-file)
(require 's)

(ert-deftest initialize ()
  (let ((client (etss-file/mocker)))
    (etss-client/initialize client)
    (with-slots (initp name) client
      (should (eq initp t))
      (should-not (s-blank? name))))
  (let* ((name "my-name")
         (client (etss-file/mocker :name name)))
    (etss-client/initialize client)
    (should (equal name (oref client name)))))

(ert-deftest contains? ()
  (let ((client (etss-file/mocker)))
    (should (etss-client/contains? client
                                  (find-file-noselect "mockdata/single-file.ts")))
    (should-not (etss-client/contains? client (current-buffer)))))
