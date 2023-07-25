;;; magitf.el --- Magit-like porcelain for terraform -*- lexical-binding: t; -*-
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
;; (require 'magitf-core)
(require 'magitf-plan)
(require 'magitf-apply)
(require 'magitf-init)
(require 'magitf-destroy)
(require 'magitf-state)

;; todo
;; Plan
;; - when init, write `terraform plan [args]' to buffer
;; - store stdout to tmpfile
;; - sentinel, when finished writing parse tmpfile with parse-tf.py
;; - write parsed lines to buffer, toggle sections with tab
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

;; Apply
;; - under the hood, always execute plan with a hidden -out
;; - write `terraform apply [args]' to buffer
;; - store stdout to tmpfle
;; - sentinel, when finished writing parse tmpfile with parse-tf.py
;; - write parsed lines to buffer, toggle sections with tab
;; - read-yes-or-no-p
;; - if user specified -out, when done, copy plan from hidden -out

;; All buffers:
;; - read q|esc as buffer-kill
;; - DRY
;; - tests?
;; - replace parse-tf.py with elisp function - test fully works as expected
;;   - error in process sentinel: string-match-p: Args out of range: "[0m[1maws_s3_bucket_policy.kubecost_policy_sandbox: Refreshing state... [id=zimpler-kubecost-billing][0m
;; - always keyboard-escape-quit/deactivate-mark after sending output or comint, unless cmd wants readprompt
;;
;; implement a magitf-mode that all buffers write to

(provide 'magitf)

;;; magitf.el ends here
