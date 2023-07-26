;;; magitf-plan.el --- functions related to plan actions  -*- lexical-binding: t; -*-

;; todo
;; Plan
;; - when init, write `terraform plan [args]' to buffer
;; - store stdout to tmpfile
;; - sentinel, when finished writing parse tmpfile with parse-tf.py
;; - write parsed lines to buffer, toggle sections with tab
;; - or catch and display on stderr, exit
;; - bugfix if unparseable regex, handle without intruding user

;; parse
;; handle replacements, they look like:
;; Terraform used the selected providers to generate the following execution plan. Resource actions are indicated with the following symbols:
;; -/+ destroy and then create replacement

;; Terraform will perform the following actions:

;;   # aws_spot_datafeed_subscription.spot_data_feed_subscription must be replaced
;; -/+ resource "aws_spot_datafeed_subscription" "spot_data_feed_subscription" {
;;       ~ id     = "spot-datafeed-subscription" -> (known after apply)
;;       + prefix = "spot" # forces replacement
;;         # (1 unchanged attribute hidden)
;;     }

;; - replace parse-tf.py with elisp function - test fully works as expected
;;   - error in process sentinel: string-match-p: Args out of range: "[0m[1maws_s3_bucket_policy.kubecost_policy_sandbox: Refreshing state... [id=zimpler-kubecost-billing][0m

(require 'magitf-core)

(transient-define-prefix magitf-infix-plan ()

  ["Arguments"
    ("-o" "Set plan path" "-out=" :always-read t)
    ("-t" "Specific target in state" "-target=" :always-read t)  ;; just append more -target= for multiple selections for now
    ("-v" "Read variables from path" "-var-file=" :always-read t)]
  ["Plan"
    ("p" "plan" magitf-suffix-plan)])

(transient-define-suffix magitf-suffix-plan (&optional args)
  "Execute `terraform plan` and catch the output."
  (interactive (list (transient-args transient-current-command)))
  (let* ((temp-file (concat (make-temp-file "magitf" t) "/plan.stdout"))
         (buffer-name "*magitf-plan*")
         (process-buffer (get-buffer-create buffer-name))
         (cmd (format "terraform plan %s 2>&1 | tee %s" (string-join args " ") temp-file)))

    ;; Execute terraform plan command and stream output to buffer and file
    (magitf--cmd-to-buffer buffer-name "terraform plan" args)
    (make-comint-in-buffer "magitf-process" process-buffer "bash" nil "-c" cmd)

    ;; watch process until done, then parse temp-file
    (set-process-sentinel
     (get-buffer-process process-buffer)
     `(lambda (process event)
        (unwind-protect
            (when (eq (process-status process) 'exit)
              (with-temp-buffer
                (magitf--parse-plan ,temp-file)))
          (unless
              (switch-to-buffer ,buffer-name)
              (deactivate-mark)))))))

;; (defun magitf--parse-plan (tmp-file)
;;   "Process a Python script file and print formatted output."
;;   (interactive "fPath to the plan.stdout file?: ")
;;   (let* ((regex "^\\(.*?place\n\\)\\(^.*?\\s{2}.*?^\\s{4}}\\)\\(?=\n\n\\)")
;;          (data (with-temp-buffer
;;                  (insert-file-contents tmp-file)
;;                  (buffer-string)))
;;          (matches (seq-map
;;                    (lambda (match)
;;                      (let ((fmt-out (replace-regexp-in-string r"(\[0m|\n)  " "\\1" (match-string 2 match))))
;;                        (replace-regexp-in-string r"\\s{2}(})" "\\1" fmt-out)))
;;                    (seq-filter
;;                     (lambda (match)
;;                       (string-match-p regex (match-string 0 match)))
;;                     (split-string data "\n\n" t)))))
;;     (dolist (match matches)
;;       match)))

;; parse-tf.py using bash and python script
(defun magitf--parse-plan (temp-file)
  "Execute parse-tf.py with TEMP-FILE and catch the output."
  (let* ((buffer-name "*magitf-parse*")
         (buffer (get-buffer-create buffer-name))
         (command (format "parse-tf.py %s" temp-file)))
   (with-current-buffer buffer
    (let ((inhibit-read-only t)) (set-text-properties (point-min) (point-max) ())))
    ;; actually dont want to put `bash -c ...' as header
    ;; (magitf--execute-cmd-in-buffer buffer-name (format "bash -c %s" command) nil)
    (make-comint-in-buffer "magitf-process" buffer "bash" nil "-c" command)
    (switch-to-buffer buffer)
    (keyboard-quit)
    (deactivate-mark)))

;; old hacky version of plan+parse together. Saved for inspiration
;; (transient-define-suffix magitf-suffix-plan (&optional args)
;;   (interactive (list (transient-args transient-current-command)))
;;   (let* ((temp-file (concat (make-temp-file "magitf" t) "/plan.stdout"))
;;          (temp-dir (file-name-directory temp-file))
;;          (parsed-file (concat temp-dir "plan.parsed"))
;;          (buffer-name "*magitf*")
;;          (process-name "magitf-process"))
;;     (unwind-protect
;;         (progn
;;           (magitf--cmd-to-buffer "terraform plan" args)
;;           (let* ((cmd (concat "terraform plan " (mapconcat #'identity args " ")
;;                               " | tee " temp-file
;;                               " && parse-tf.py " temp-file
;;                               " > " parsed-file)))
;;             (start-process-shell-command process-name buffer-name cmd)))
;;       (set-process-sentinel
;;        (get-buffer-process buffer-name)
;;        `(lambda (process event)
;;           (when (eq (process-status process) 'exit)
;;             (magitf--file-content-to-buffer ,parsed-file)
;;             (delete-directory temp-dir t)
;;             (magitf--set-buffer-read-only)
;;             (deactivate-mark)))))))

(provide 'magitf-plan)

;; magitf-plan.el ends here
