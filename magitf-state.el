;;; magitf-state.el --- functions related to state actions  -*- lexical-binding: t; -*-

(require 'magitf-core)

(transient-define-prefix magitf-infix-state-list ()

  ["State list"
   ("c" "Copy to clipboard from selection" magitf-suffix-state-list-copy)
   ("l" "Display all targets" magitf-suffix-state-list)])

(transient-define-prefix magitf-infix-state ()

  ["State"
   ("l" "list" magitf-infix-state-list)
   ("p" "pull" magitf-suffix-state-pull)
   ("r" "rm" magitf-suffix-state-rm) ; testme
   ("s" "show" magitf-suffix-state-show)])

(transient-define-suffix magitf-suffix-state-pull (&optional args)
  (interactive (list (transient-args transient-current-command)))
  (magitf--execute-cmd-in-buffer "*magitf-state*" (format "terraform state pull %s" (string-join args " ")) nil))

(transient-define-suffix magitf-suffix-state-show ()
  (interactive)
  (magitf--read-terraform--state-show)
  (magitf--set-buffer-read-only "*magitf*")
  (deactivate-mark))

(transient-define-suffix magitf-suffix-state-rm ()
  (interactive)
  ;; should not be read only, state rm prompts for yes/no
  (magitf--read-terraform--state-rm))
  ;; (magitf--set-buffer-read-only "*magitf*")
  ;; (deactivate-mark))

(transient-define-suffix magitf-suffix-state-list ()
  (interactive)
  (magitf--read-terraform--state-list))

(transient-define-suffix magitf-suffix-state-list-copy ()
  (interactive)
  (magitf--read-terraform--state-list-copy))

;; Does not have any error handling
;; (defun magitf--read-terraform--state-list ()
;;   "get list of state and prompt user for a selection to copy to clipboard."
;;   (interactive)
;;   (let* ((command "terraform state list")
;;          (output (shell-command-to-string command))
;;          (states (split-string output "\n" t))
;;          (selected-state (completing-read "Select a Terraform state: " states)))
;;     (kill-new selected-state)
;;     (message "Selected state copied to clipboard: %s" selected-state)))

(defun magitf--read-terraform--state-list ()
  "Execute `terraform state list` and catch the output."
  (interactive)
  (magitf--execute-cmd-in-buffer "*magitf-unlock*" "terraform state list" nil))

(defun magitf--read-terraform--state-list-copy ()
  "Copy selection to clipboard by prompting user for state from a list of states."
  ;; find a way to abort / msg user on error
  (interactive)
  (let* ((selected-state (magitf--read-terraform--select-state)))
    (kill-new selected-state)
    (message "Copied: `%s`" selected-state)))

(defun magitf--read-terraform--state-show ()
  "Show selected state by prompting user for selection from a list of states."
  ;; add error handling
  (interactive)
  (magitf--execute-cmd-in-buffer "*magitf-state*" (format "terraform state show %s" (magitf--read-terraform--select-state)) nil))

(defun magitf--read-terraform--state-rm ()
  "Remove selected state by prompting user for selection from a list of states."
  (interactive)
  (magitf--execute-cmd-in-buffer "*magitf-unlock*" (format "terraform state rm %s" (magitf--read-terraform--select-state)) nil))

(defun magitf--read-terraform--select-state ()
  (interactive)
  (let* ((command "terraform state list")
         (output (ansi-color-apply (shell-command-to-string command)))
         (states (split-string output "\n" t))
         (selected-state (completing-read "Select a Terraform state: " states)))
    selected-state))

(provide 'magitf-state)

;; magitf-state.el ends here
