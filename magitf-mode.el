;;; magitf-mode.el --- Base mode for magitf.  -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(require 'magit-section)
(require 'subr-x)

(autoload 'magitf-infix-init "config")
(autoload 'magitf-infix-plan "config")
(autoload 'magitf-infix-apply "config")
(autoload 'magitf-infix-state "config")
(autoload 'magitf-infix-state "config")

(defvar magitf-mode-map
  (let ((keymap (make-sparse-keymap)))
    ;; Section controls
    (define-key keymap (kbd "p")   #'magit-section-backward)
    (define-key keymap (kbd "n")   #'magit-section-forward)
    (define-key keymap (kbd "M-p") #'magit-section-backward-sibling)
    (define-key keymap (kbd "M-n") #'magit-section-forward-sibling)
    (define-key keymap (kbd "C-i") #'magit-section-toggle)
    (define-key keymap (kbd "^")   #'magit-section-up)
    (define-key keymap [tab]       #'magit-section-toggle)
    ;; (define-key keymap [C-tab]     #'magit-section-cycle)
    ;; (define-key keymap [M-tab]     #'magit-section-cycle-diffs)
    ;; (define-key keymap [S-tab]     #'magit-section-cycle-global)
    ;; Misc
    (define-key keymap (kbd "q") #'quit-window)
    ;; (define-key keymap (kbd "Q") #'magitf-kill-buffers-and-processes)  ; implement
    ;; (define-key keymap (kbd "RET") #'magitf-navigate)
    ;; (define-key keymap (kbd "M-w") #'magitf-copy-thing-at-point)
    (define-key keymap (kbd "h") #'describe-mode)

    (define-key keymap (kbd "i") #'magitf-infix-init)
    (define-key keymap (kbd "p") #'magitf-infix-plan)
    (define-key keymap (kbd "s") #'magitf-infix-state)
    ;; (define-key keymap (kbd "P") #'magitf-proxy)
    ;; (define-key keymap (kbd "?") #'magitf-dispatch)
    ;; (define-key keymap (kbd "c") #'magitf-config-popup)
    ;; (define-key keymap (kbd "C") #'magitf-context)
    ;; (define-key keymap (kbd "d") #'magitf-describe)
    ;; (define-key keymap (kbd "D") #'magitf-mark-for-delete)
    ;; (define-key keymap (kbd "e") #'magitf-exec)
    ;; (define-key keymap (kbd "E") #'magitf-edit)
    ;; (define-key keymap (kbd "f") #'magitf-file)
    ;; (define-key keymap (kbd "g") #'magitf-refresh)
    ;; (define-key keymap (kbd "l") #'magitf-logs)
    ;; (define-key keymap (kbd "L") #'magitf-labels)
    ;; (define-key keymap (kbd "u") #'magitf-unmark)
    ;; (define-key keymap (kbd "U") #'magitf-unmark-all)
    ;; (define-key keymap (kbd "x") #'magitf-execute-marks)

    keymap)
  "Keymap for `magitf-mode'.  This is the base keymap for all derived modes.")

;;;###autoload
(define-derived-mode magitf-mode special-mode "Magitf"
  "Base mode for Magitf modes.

\\{magitf-mode-map}"
  :group 'magitf
  (read-only-mode)
  (buffer-disable-undo)
  (setq truncate-lines t)
  (setq-local line-move-visual t)
  (setq show-trailing-whitespace nil)
  (setq list-buffers-directory (abbreviate-file-name default-directory))
  (hack-dir-local-variables-non-file-buffer)
  (make-local-variable 'text-property-default-nonsticky)
  (push (cons 'keymap t) text-property-default-nonsticky)
  (push (cons 'magitf-nav t) text-property-default-nonsticky)
  (push (cons 'magitf-copy t) text-property-default-nonsticky)
  (add-hook 'post-command-hook #'magit-section-update-highlight t t)
  (when (bound-and-true-p global-linum-mode)
    (linum-mode -1))
  (when (and (fboundp 'nlinum-mode)
             (bound-and-true-p global-nlinum-mode))
    (nlinum-mode -1)))

(provide 'magitf-mode)

;;; magitf-mode.el ends here
