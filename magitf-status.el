;;; magitf-status.el --- status-mode                 -*- lexical-binding: t; -*-

;; Copyright (C) 2023  Johan Radivoj

;; Author: Johan Radivoj <johan@radivoj.se>

(require 'magitf-core)

(transient-define-prefix magitf-status ()
  "Magitf - A magit-like porcelain for terraform."

   ["Terraform"
    ["Actions"
     ; ("p" "plan" magitf--suffix-plan :transient transient--do-call)
     ; ("i" "init" magitf-suffix-placeholder :transient transient--do-call)
     ("i" "init" magitf-infix-init)
     ("v" "validate" magitf-suffix-validate)
     ("p" "plan" magitf-infix-plan)
     ("a" "apply" magitf-infix-apply)
     ("d" "destroy" magitf-infix-destroy)]
    ["Utils"
     ("c" "console" magitf-suffix-console)
     ("f" "fmt" magitf-infix-fmt)
     ("s" "state" magitf-infix-state)
     ("u" "force-unlock" magitf-suffix-force-unlock)]
     ; ("m" "import" magitf-suffix-placeholder)]  ; not yet implemented
    ["Controls"
     ("b" "delete buffer" magitf--quit-buffer)
     ("q" "quit" keyboard-escape-quit)]
   ])

(provide 'magitf-status)

;; magitf-status.el ends here
