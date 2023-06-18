;;; config.el -                                    -*- lexical-binding: t; -*-

;; Copyright (C) 2023  Johan Radivoj

;; Author: Johan Radivoj <johan@radivoj.se>
;; Keywords: lisp,

(defvar +terraform-runner (if (executable-find "terragrunt") "terragrunt" "terraform")
  "The default runner - terraform or terragrunt")

(when (modulep! :tools magitf)
 (map! :leader
   (:prefix-map ("g" . "git")
          :desc "magitf status"    "T"   #'magitf-status))
 (map! :map terraform-mode-map
       :localleader
       :desc "terraform" "t" #'magitf-status))

(when (modulep! +lsp)
  (add-hook 'terraform-mode-local-vars-hook #'lsp! 'append))

(eval-when-compile (require 'subr-x))

(use-package transient-showcase)

(transient-define-prefix magitf-infix-init ()

  ["Init"
   ["Options"
    ("r" "reconfigure" "-reconfigure")
    ("u" "upgrade" "-upgrade")
    ("m" "migrate-state" "-migrate-state")
    ""
    ("i" "init" magitf-suffix-init)
   ]
  ])

(transient-define-prefix magitf-infix-plan ()

  ["Plan"
   ["Options"
    ("o" "out" "-out=" :always-read t)
    ("t" "target" "-target=" :always-read t)
    ("v" "var-file" "-var-file=" :always-read t)
    ""
    ("p" "plan" magitf-suffix-plan)
   ]
  ])

(transient-define-prefix magitf-infix-apply ()

  ["Apply"
   ["Options"
    ("t" "target" "-target=" :always-read t)
    ("v" "var-file" "-var-file=" :always-read t)
    ""
    ("a" "apply" magitf-suffix-apply)
   ]
  ])

(transient-define-prefix magitf-infix-destroy ()

  ["Destroy"
   ["Options"
    ("t" "target" "-target=" :always-read t)
    ""
    ("d" "destroy" magitf-suffix-destroy)
   ]
  ])

(transient-define-prefix magitf-infix-state ()

  ["state"
   ["Options"
    ("l" "list" magitf-suffix-placeholder)
    ("p" "pull" magitf-suffix-placeholder)
    ("r" "rm" magitf-suffix-placeholder)
    ("s" "show" magitf-suffix-placeholder)
   ]
  ])

(transient-define-prefix magitf-status ()
  "magitf - porcelain for temacsrraform"

  ; only one kind of eyes is meaningful at a time
  ; :incompatible '(("-b" "-g" "-p" "-s" "-t" "-w" "-y"))

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
     ("f" "fmt" magitf-suffix-fmt)
     ("s" "state" magitf-infix-state)
     ("u" "force-unlock" magitf-suffix-force-unlock)]
     ; ("m" "import" magitf-suffix-placeholder)]  ; not yet implemented
    ["\n"]
    ["Controls"
     ("b" "delete buffer" magitf--quit-buffer)
     ("q" "quit" evil-escape)]
   ])

(transient-define-suffix magitf--quit-buffer ()
  "Kill the magitf buffer and exit."
  (interactive)
  (ignore-errors (kill-buffer "*magitf*"))
  (message "magitf buffer destroyed"))

(defun magitf--buffer-exists-p ()
  "Visibility predicate."
  (not (equal (get-buffer "*magitf*") nil)))

; found this from some working example but didn't get it working here
;; (transient-define-suffix magitf--clear-buffer (&optional buffer)
;;   "Delete the *magitf* buffer.  Optional BUFFER name."
;;   :transient 'transient--do-call
;;   :if 'magitf--buffer-exists-p
;;   (interactive) ; todo look at "b" interactive code

;;   (save-excursion
;;     (let ((buffer (or buffer "*magitf*")))
;;       (set-buffer buffer)
;;       (delete-region 1 (+ 1 (buffer-size))))))

;; (defun execute-commands (buffer &rest commands)
;;   "Execute a list of shell commands sequentially"
;;   (with-current-buffer buffer
;;     (set (make-local-variable 'commands-list) commands)
;;     (start-next-command)))

;; (defun start-next-command ()
;;   "Run the first command in the list"
;;   (if (null commands-list)
;;       (insert "\nDone.")
;;     (let ((command  (car commands-list)))
;;       (setq commands-list (cdr commands-list))
;;       (insert (format ">>> %s\n" command))
;;       (let ((process (start-process-shell-command command (current-buffer) command)))
;;         (set-process-sentinel process 'sentinel)))))

;; (defun sentinel (p e)
;;   "After a process exited, call `start-next-command' again"
;;   (let ((buffer (process-buffer p)))
;;     (when (not (null buffer))
;;       (with-current-buffer buffer
;;         ;(insert (format "Command `%s' %s" p e) )
;;         (start-next-command)))))

  ; (ansi-color-apply-on-region (window-start) (window-end) t)
    ;; (when (magitf--buffer-exists-p)
    ;;   (magitf--clear-buffer))
    ; (start-process "terraform" "*magitf*" "terraform" "plan")
    ;; (cmd! compile (format "%s plan" +terraform-runner)))
    ; (switch-to-buffer "*magitf*"))

(transient-define-suffix magitf-suffix-init(&optional args)
  (interactive (list (transient-args transient-current-command)))
  (magitf--cmd-to-buffer "terraform init" args)
  (apply #'make-comint-in-buffer "magitf" nil "terraform" nil "init" args)
  (magitf--set-buffer-read-only)
  (evil-escape)
  )

(transient-define-suffix magitf-suffix-validate (&optional args)
  (interactive (list (transient-args transient-current-command)))
  (magitf--cmd-to-buffer "terraform validate" args)
  (apply #'make-comint-in-buffer "magitf" nil "terraform" nil "validate" args)
  (magitf--set-buffer-read-only)
  (evil-escape)
  )

(transient-define-suffix magitf-suffix-fmt(&optional args)
  (interactive (list (transient-args transient-current-command)))
  (magitf--cmd-to-buffer "terraform fmt" args)
  (apply #'make-comint-in-buffer "magitf" nil "terraform" nil "fmt" args)
  (magitf--set-buffer-read-only)
  (evil-escape)
  )

;; this is a working example - dont mess it up
;; (transient-define-suffix magitf-suffix-plan (&optional args)
;;   (interactive (list (transient-args transient-current-command)))
;;   (with-current-buffer (get-buffer-create "*magitf*")
;;    (let ((inhibit-read-only t)) (set-text-properties (point-min) (point-max) ()))
;;    (read-only-mode -1) (erase-buffer))
;;   (with-current-buffer "*magitf*"
;;     ;(insert (propertize (format ">>> terraform plan %s\n" (or (string-join args "") "")) 'read-only t)))
;;     (insert (format ">>> terraform plan %s\n" (or (string-join args "") "")) ))
;;   (apply #'make-comint-in-buffer "magitf" nil "terraform" nil "plan" args)
;;   (with-current-buffer "*magitf*" (read-only-mode 1))
;;   (switch-to-buffer "*magitf*")
;;   (evil-escape))

(transient-define-suffix magitf-suffix-plan (&optional args)
  (interactive (list (transient-args transient-current-command)))
  (magitf--cmd-to-buffer "terraform plan" args)
  (apply #'make-comint-in-buffer "magitf" nil "terraform" nil "plan" args)
  (magitf--set-buffer-read-only)
  (evil-escape)
  )

(transient-define-suffix magitf-suffix-apply (&optional args)
  (interactive (list (transient-args transient-current-command)))
  (magitf--cmd-to-buffer "terraform apply" args)
  (apply #'make-comint-in-buffer "magitf" nil "terraform" nil "apply" args)
  ; Should not be read only because eventually tf prompts requiring input.
  ; Can we instead use read-y-or-n and input into the tf prompt? (for y/n instead of arbitrary anything allowed to be input atm)
  (magitf--set-buffer-write)
  )

(transient-define-suffix magitf-suffix-destroy (&optional args)
  (interactive (list (transient-args transient-current-command)))
  (magitf--cmd-to-buffer "terraform destroy" args)
  (apply #'make-comint-in-buffer "magitf" nil "terraform" nil "destroy" args)
  ; Should not be read only because eventually tf prompts requiring input.
  ; Can we instead use read-y-or-n and input into the tf prompt? (for y/n instead of arbitrary anything allowed to be input atm)
  (magitf--set-buffer-write)
  )

(transient-define-suffix magitf-suffix-console (&optional args)
  (interactive (list (transient-args transient-current-command)))
  (magitf--cmd-to-buffer "terraform console" args)
  (apply #'make-comint-in-buffer "magitf" nil "terraform" nil "console" args)
  ; Should not be read only because this is sorta a REPL which we want to continue interaction with
  (magitf--set-buffer-write)
  )

(transient-define-suffix magitf-suffix-force-unlock (id)
  ; TODO: find a way to require input. Or use completing-read and populate with some auto-fetched state->id mapping for better UX
  (interactive "sID?: ")
  (magitf--cmd-to-buffer "terraform force-unlock" (list id))
  (apply #'make-comint-in-buffer "magitf" nil "terraform" nil (list "force-unlock" id))

  ; Should not be read only because eventually tf prompts requiring input.
  ; Can we instead use read-y-or-n and input into the tf prompt? (for y/n instead of arbitrary anything allowed to be input atm)
  ; (with-current-buffer "*magitf*" (read-only-mode 1))
  (magitf--set-buffer-write)
  )

(defun magitf--set-buffer-write ()
  (with-current-buffer "*magitf*" (read-only-mode -1)))

(defun magitf--set-buffer-read-only ()
  (with-current-buffer "*magitf*" (read-only-mode 1)))

(defun magitf--reset-buffer ()
  (with-current-buffer (get-buffer-create "*magitf*")
   (let ((inhibit-read-only t)) (set-text-properties (point-min) (point-max) ()))
   (read-only-mode -1) (erase-buffer)
  ))

(defun magitf--cmd-to-buffer (cmd-input &optional args)
  (magitf--reset-buffer)
  (with-current-buffer (get-buffer-create "*magitf*")
    (insert (format ">>> %s %s\n" cmd-input (or (string-join args "") "")) ))
  ;; I thought this would work to make-comint-in-buffer, but I get some unknown listp <subcmd> I dont understand
  ; (setq cmd (nth 0 (split-string cmd-input)))
  ; (setq subcmd (nth 1 (split-string cmd-input)))
  ; we want to combine args and id into one apply in this func but I'm not getting it to work...
  ; (apply #'make-comint-in-buffer "magitf" nil cmd nil (list subcmd (or args "")))
  (magitf--set-buffer-write)
  (switch-to-buffer "*magitf*")
  )

(defun magitf-suffix-placeholder ()
  "Wave at the user."
  (interactive)
  (message "Waves at the user at: %s." (current-time-string)))

;; clever code I've found to maybe be reused
;; (when (magitf--buffer-exists-p)
;;   (magitf--clear-buffer))
;; (set-process-sentinel (start-process "terraform" buffer ("terraform" "plan" args)) #'output-message-sentinel)

;; (defun output-message-sentinel (process msg)
;;   (when (memq (process-status process) '(exit signal))
;;     (message (concat (process-name process) " - " msg))))
; (apply #'cmd! (compile (format "terraform plan %s" args)))))
