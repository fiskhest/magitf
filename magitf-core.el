;;; magitf-core.el --- core functions -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

;;;; Requirements
(require 'transient)
(require 'ansi-color)
(eval-when-compile (require 'subr-x))

;; test using this as a wrapper for executing command and setting read state
;; random showerthoughts:
;; can we wrap unwind-protect along with set-process-sentinel to catch output and hide confusing errors in the background?
(defun magitf--execute-cmd-in-buffer (buffer-name cmd-input write-mode)
  "Send a string representing the command and then execute the command in a buffer.
Argument BUFFER-NAME The buffer to send command to
Argument CMD-INPUT The command and any subcmd as a string.
Argument write-mode Set the buffer to read-only (nil) or write (t)."
  (let ((cmd-in (split-string cmd-input)))
    ;; (let ((cmd (car cmd-in))
    ;;      (subcmd (car (cdr cmd-in)))
    ;;      (args (cdr (cdr cmd-in))))
    (let ((cmd (nth 0 cmd-in))
          (subcmd (nth 1 cmd-in))
          (args (cdr (cdr cmd-in))))
   (magitf--cmd-to-buffer buffer-name cmd-input)
   (apply #'make-comint-in-buffer "magitf" buffer-name cmd nil subcmd args)
   (if write-mode
       (magitf--set-buffer-write buffer-name)
     :else
       (magitf--set-buffer-read-only buffer-name)
       (deactivate-mark)))))

(defun magitf--read-string (&optional prompt)
  "Reads user inputed string with optional prompt set. Requires non-empty input."
  (let ((input ""))
    (while (string-empty-p input)
      (setq input (read-string prompt))
      (if (string-empty-p input)
          (message "String must not be empty, please input data")))
    input))

(defun magitf--set-buffer-write (buffer-name)
  "Unlocks the buffer for writing new data into."
  (with-current-buffer buffer-name
    (read-only-mode -1)))
  ;; (message (format "%s: buffer inputmode" buffer-name)))

(defun magitf--set-buffer-read-only (buffer-name)
  "Locks the buffer read-only for writing new data into."
  (with-current-buffer buffer-name
    (read-only-mode 1)
    (deactivate-mark)))
  ;; (sit-for 0.1)
  ;; (keyboard-quit))
  ;; (message (format "%s: buffer read only" buffer-name)))

(defun magitf--reset-buffer (buffer-name)
  "Wipe the buffer."
  (with-current-buffer (get-buffer-create buffer-name)
    (let ((inhibit-read-only t))
      (set-text-properties (point-min) (point-max) ()))
    (read-only-mode -1)
    (erase-buffer)))

(defun magitf--cmd-to-buffer (buffer-name cmd-input &optional args)
  "Write intended cmd to buffer.
Argument CMD-INPUT The command and any subcmd as a string.
Optional argument ARGS Any additional arguments to the command."
  (magitf--reset-buffer buffer-name)
  (with-current-buffer (get-buffer-create buffer-name)
    (insert (format ">>> %s %s\n" cmd-input (or (string-join args "") "")) ))
  (magitf--set-buffer-write buffer-name)
  (switch-to-buffer buffer-name))

(defun magitf--file-content-to-buffer (file-path buffer-name)
  "Write contents of file to buffer.
Argument FILE-PATH path of the file for which to read the contents.
Argument BUFFER-NAME the buffer to send file contents to."
  (magitf--reset-buffer buffer-name)
  (with-current-buffer (get-buffer-create buffer-name)
    (insert-file-contents file-path)
    (ansi-color-apply-on-region (point-min) (point-max)))
  (magitf--set-buffer-write buffer-name)
  ;; (message file-path)
  (switch-to-buffer buffer-name))

(defun magitf-suffix-placeholder ()
  "Placeholder, wave at the user, like `tsc-suffix-wave'."
  (interactive)
  (message "Waves at the user at: %s." (current-time-string)))

(provide 'magitf-core)

;;; magitf-core.el ends here
