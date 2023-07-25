;;; magitf-init.el --- functions related to init actions  -*- lexical-binding: t; -*-

(require 'magitf-core)

(transient-define-prefix magitf-infix-init ()

  ["Arguments"
    ("-r" "Reconfigure state" "-reconfigure")
    ("-u" "Upgrade modules" "-upgrade")
    ("-m" "Migrate state between backends" "-migrate-state")]
  ["Init"
    ("i" "init" magitf-suffix-init)])

(transient-define-suffix magitf-suffix-init (&optional args)
  "Execute `terraform init` and catch the output."
  (interactive (list (transient-args transient-current-command)))
  (magitf--execute-cmd-in-buffer "*magitf-init*" (format "terraform init %s" (string-join args " ")) nil))

(provide 'magitf-init)

;; magitf-init.el ends here
