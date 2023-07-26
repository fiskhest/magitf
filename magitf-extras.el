;;; magitf-extras.el --- other actions that cannot be grouped elsewhere -*- lexical-binding: t; -*-

;; Copyright (C) 2023  Johan Radivoj

;; Author: Johan Radivoj <johan@radivoj.se>
;; Keywords: lisp

(require 'magitf-core)

(transient-define-prefix magitf-infix-fmt ()

  ["State"
   ("-d" "Display diffs of formatting changes" "-diff")
   ("-c" "Check if input is properly formatted and send a notification" "-check")
   ("-r" "Recurse through subdirectories" "-recursive")
   ""
   ("f" "fmt" magitf-suffix-fmt)])

(transient-define-suffix magitf-suffix-validate ()
  "Execute `terraform validate` and catch the output."
  ;; add infix+/support for -json
  (interactive)
  (magitf--execute-cmd-in-buffer "*magitf-validate*" "terraform validate" nil)) ;; or t

(transient-define-suffix magitf-suffix-fmt (&optional args)
  "Execute `terraform fmt` and catch the output."
  (interactive (list (transient-args transient-current-command)))
  (magitf--execute-cmd-in-buffer "*magitf-fmt*" (format "terraform fmt %s" (string-join args " ")) nil)) ;; or t

(transient-define-suffix magitf-suffix-console (&optional args)
  (interactive (list (transient-args transient-current-command)))
  ; verify with -help that console takes any args
  ; write-mode as this is sorta a REPL which we want to continue interaction with
  (magitf--execute-cmd-in-buffer "*magitf-console*" (format "terraform console %s" (string-join args " ")) t))

(transient-define-suffix magitf-suffix-force-unlock ()
  (interactive)
  (let ((input-str (magitf--read-string "Enter ID: ")))
  ; Has to be write because eventually tf prompts requiring input.
  ; Can we instead use read-y-or-n and input into the tf prompt? (for y/n instead of arbitrary anything allowed to be input atm)
    (magitf--execute-cmd-in-buffer "*magitf-unlock*" (format "terraform force-unlock %s" input-str) t)))

(defun magitf-suffix-import ()
  "Import a remote object already provisioned into state."
  (interactive)
  (let ((resource-addr (magitf--read-string "State resource address?: "))
        (resource-id (magitf--read-string "Provider resource object identifier?: ")))
    (magitf--execute-cmd-in-buffer "*magitf-import*" (format "terraform import %s %s" resource-addr resource-id) nil)))

(transient-define-suffix magitf--quit-buffer ()
  "Kill the magitf buffer and exit."
  (interactive)
  (ignore-errors (kill-buffer "*magitf*"))
  (message "magitf buffer destroyed"))

(provide 'magitf-extras)

;; magitf-extras.el ends here
