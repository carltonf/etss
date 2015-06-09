(require 'etss-manager)

(ert-deftest client-loaded? ()
  (let ((etss-manager/client-list (etss-manager/client-list-mocker)))
    (should (etss-manager/client-loaded? (find-file-noselect "mockdata/single-file.ts")))
    (should-not (etss-manager/client-loaded? (current-buffer)))))

(ert-deftest get-client-class ()
  (should (eq (etss-manager/get-client-class (find-file-noselect "mockdata/single-file.ts"))
              'etss-file/class)))
