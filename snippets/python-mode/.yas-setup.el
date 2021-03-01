
;; based on a snippet by Xaldew: https://emacs.stackexchange.com/a/19471/27115
;; Revised by A. Bochkarev

(defun python-args-to-google-docstring (text)
  "Return a Google style docstring format for the python arguments in yas-text."
  (let* ((indent (concat "\n" (make-string (current-column) 32)))
         (args (python-split-args text))
         (nr 0)
         (formatted-args
          (mapconcat
           (lambda (x)
             (concat "   "
                     (nth 0 x)
                     " (${type}):"
                     " ${description}"
                     (if (nth 1 x) (concat " \(default " (nth 1 x) "\)"))))
           args
           indent)))
    (unless (string= formatted-args "")
      (concat
       (mapconcat 'identity
                  (list "" "Args:" formatted-args)
                  indent)
       "\n"))))
