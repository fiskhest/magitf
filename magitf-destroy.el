;;; magitf-destroy.el --- functions related to destroy actions  -*- lexical-binding: t; -*-

(require 'magitf-core)

(transient-define-prefix magitf-infix-destroy ()

  ["Arguments"
   ("-t" "Specific target in state" "-target=" :always-read t)]
  ["Destroy"
   ("d" "destroy" magitf-suffix-destroy)])

(transient-define-suffix magitf-suffix-destroy (&optional args)
  (interactive (list (transient-args transient-current-command)))
  ; Has to be write because eventually tf prompts requiring input.
  ; Can we instead use read-y-or-n and input into the tf prompt? (for y/n instead of arbitrary anything allowed to be input atm)
  (magitf--execute-cmd-in-buffer "*magitf-destroy*" (format "terraform destroy %s" (string-join args " ")) t))

(provide 'magitf-destroy)

;; magitf-destroy.el ends here
