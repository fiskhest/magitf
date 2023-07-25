;;; magitf-apply.el --- functions related to apply actions  -*- lexical-binding: t; -*-

(require 'magitf-core)

(transient-define-prefix magitf-infix-apply ()

  ["Arguments"
    ("-t" "Specific target in state" "-target=" :always-read t)
    ("-v" "Read variables from path" "-var-file=" :always-read t)]
  ["Apply"
    ("a" "apply" magitf-suffix-apply)])

(transient-define-suffix magitf-suffix-apply (&optional args)
  (interactive (list (transient-args transient-current-command)))
  ; Has to be write because eventually tf prompts requiring input.
  ; Can we instead use read-y-or-n and input into the tf prompt? (for y/n instead of arbitrary anything allowed to be input atm)
  (magitf--execute-cmd-in-buffer "*magitf-apply*" (format "terraform apply %s" (string-join args " ")) t))

(provide 'magitf-apply)

;; magitf-apply.el ends here
