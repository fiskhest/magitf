;;; magitf.el --- A terraform porcelain -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2023  Johan Radivoj
;;
;; Author: Johan Radivoj <https://github.com/fiskhest>
;; Maintainer: Johan Radivoj
;; Version: 20230710.1
;; Homepage: https://github.com/fiskhest/magitf.el
;; Package-Requires: ((emacs "28.1"))

;; This file is NOT part of GNU Emacs.

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; magitf.el provides a magit-like interactive experience for terraform in Emacs.

;;; Code:

;;;; Requirements
(require 'transient)
(eval-when-compile (require 'subr-x))

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
  "Magitf - porcelain for temacsrraform."

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
    ["Controls"
     ("b" "delete buffer" magitf--quit-buffer)
     ("q" "quit" keyboard-escape-quit)]
   ])

(transient-define-suffix magitf--quit-buffer ()
  "Kill the magitf buffer and exit."
  (interactive)
  (ignore-errors (kill-buffer "*magitf*"))
  (message "magitf buffer destroyed"))

(defun magitf--buffer-exists-p ()
  "Visibility predicate."
  (not (equal (get-buffer "*magitf*") nil)))

(transient-define-suffix magitf-suffix-init (&optional args)
  (interactive (list (transient-args transient-current-command)))
  (magitf--cmd-to-buffer "terraform init" args)
  (apply #'make-comint-in-buffer "magitf" nil "terraform" nil "init" args)
  (magitf--set-buffer-read-only)
  (keyboard-escape-quit))

(transient-define-suffix magitf-suffix-validate (&optional args)
  (interactive (list (transient-args transient-current-command)))
  (magitf--cmd-to-buffer "terraform validate" args)
  (apply #'make-comint-in-buffer "magitf" nil "terraform" nil "validate" args)
  (magitf--set-buffer-read-only)
  (keyboard-escape-quit))

(transient-define-suffix magitf-suffix-fmt (&optional args)
  (interactive (list (transient-args transient-current-command)))
  (magitf--cmd-to-buffer "terraform fmt" args)
  (apply #'make-comint-in-buffer "magitf" nil "terraform" nil "fmt" args)
  (magitf--set-buffer-read-only)
  (keyboard-escape-quit))

(transient-define-suffix magitf-suffix-plan (&optional args)
  (interactive (list (transient-args transient-current-command)))
  (magitf--cmd-to-buffer "terraform plan" args)
  (apply #'make-comint-in-buffer "magitf" nil "terraform" nil "plan" args)
  (magitf--set-buffer-read-only)
  (keyboard-escape-quit))

(transient-define-suffix magitf-suffix-apply (&optional args)
  (interactive (list (transient-args transient-current-command)))
  (magitf--cmd-to-buffer "terraform apply" args)
  (apply #'make-comint-in-buffer "magitf" nil "terraform" nil "apply" args)
  ; Should not be read only because eventually tf prompts requiring input.
  ; Can we instead use read-y-or-n and input into the tf prompt? (for y/n instead of arbitrary anything allowed to be input atm)
  (magitf--set-buffer-write))

(transient-define-suffix magitf-suffix-destroy (&optional args)
  (interactive (list (transient-args transient-current-command)))
  (magitf--cmd-to-buffer "terraform destroy" args)
  (apply #'make-comint-in-buffer "magitf" nil "terraform" nil "destroy" args)
  ; Should not be read only because eventually tf prompts requiring input.
  ; Can we instead use read-y-or-n and input into the tf prompt? (for y/n instead of arbitrary anything allowed to be input atm)
  (magitf--set-buffer-write))

(transient-define-suffix magitf-suffix-console (&optional args)
  (interactive (list (transient-args transient-current-command)))
  (magitf--cmd-to-buffer "terraform console" args)
  (apply #'make-comint-in-buffer "magitf" nil "terraform" nil "console" args)
  ; Should not be read only because this is sorta a REPL which we want to continue interaction with
  (magitf--set-buffer-write))

(transient-define-suffix magitf-suffix-force-unlock (id)
  ; TODO: find a way to require input (currently RET ing empty input becomes `tf force-unlock ""'
  ; Or use completing-read and pre-populate with some auto-fetched state->id mapping for better UX
  (interactive "sID?: ")
  (magitf--cmd-to-buffer "terraform force-unlock" (list id))
  (apply #'make-comint-in-buffer "magitf" nil "terraform" nil (list "force-unlock" id))

  ; Should not be read only because eventually tf prompts requiring input.
  ; Can we instead use read-y-or-n and input into the tf prompt? (for y/n instead of arbitrary anything allowed to be input atm)
  ; (with-current-buffer "*magitf*" (read-only-mode 1))
  (magitf--set-buffer-write))

(defun magitf--set-buffer-write ()
  "Unlocks the buffer for writing new data into."
  (with-current-buffer "*magitf*" (read-only-mode -1)))

(defun magitf--set-buffer-read-only ()
  "Locks the buffer read-only for writing new data into."
  (with-current-buffer "*magitf*" (read-only-mode 1)))

(defun magitf--reset-buffer ()
  "Reset data in the buffer."
  (with-current-buffer (get-buffer-create "*magitf*")
   (let ((inhibit-read-only t)) (set-text-properties (point-min) (point-max) ()))
   (read-only-mode -1) (erase-buffer)))

(defun magitf--cmd-to-buffer (cmd-input &optional args)
  "Write intended cmd to buffer.
Argument CMD-INPUT The command and any subcmd as a string.
Optional argument ARGS Any additional arguments to the command."
  (magitf--reset-buffer)
  (with-current-buffer (get-buffer-create "*magitf*")
    (insert (format ">>> %s %s\n" cmd-input (or (string-join args "") "")) ))
  ;; I thought this would work to make-comint-in-buffer, but I get some unknown listp <subcmd> I dont understand
  ; (setq cmd (nth 0 (split-string cmd-input)))
  ; (setq subcmd (nth 1 (split-string cmd-input)))
  ; we want to combine args and id into one apply in this func but I'm not getting it to work...
  ; (apply #'make-comint-in-buffer "magitf" nil cmd nil (list subcmd (or args "")))
  (magitf--set-buffer-write)
  (switch-to-buffer "*magitf*"))

(defun magitf-suffix-placeholder ()
  "Placeholder, wave at the user, like `tsc-suffix-wave'."
  (interactive)
  (message "Waves at the user at: %s." (current-time-string)))

(provide 'magitf)
;;; magitf.el ends here
