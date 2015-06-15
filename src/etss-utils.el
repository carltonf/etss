;;; Various utility&helper functions

(eval-when-compile
  (require 'cl))

(defun etss-utils/assoc-path (alist keys)
  "ALIST is a nested associate list, KEYS are a list of of keys or a single key.
Search KEYS in ALIST in a similar manner like XPATH. Return the
value after all KEYS found, or nil if some keys are not found.

This function is quite safe, i.e. any errors will only yield a nil return."
  (ignore-errors
    (unless (listp keys)
      (setq keys (list keys)))
    (loop for key in keys
          for nalist = (assoc key alist)
          if nalist
          do (setq alist (cdr nalist))
          else do (setq alist nil))
    alist))

(provide 'etss-utils)
