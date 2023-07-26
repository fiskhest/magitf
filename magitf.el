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
(require 'magitf-extras)
(require 'magitf-status)

;; All buffers:
;; - read q|esc as buffer-kill
;; - DRY
;; - tests?
;; - always keyboard-escape-quit/deactivate-mark after sending output or comint, unless cmd wants readprompt
;;
;; implement a magitf-mode that all buffers write to

(provide 'magitf)

;;; magitf.el ends here
