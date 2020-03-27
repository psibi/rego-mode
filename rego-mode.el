;;; rego-mode.el --- a major mode for rego configuration language -*- lexical-binding: t -*-

;; Copyright (C) 2017 Sibi Prabakaran

;; Author: Sibi Prabakaran <sibi@psibi.in>
;; Maintainer: Sibi Prabakaran <sibi@psibi.in>
;; Keywords: languages
;; Version: 0.1.3
;; Package-Requires: ((emacs "24.4") (reformatter "0.3"))
;; URL: https://github.com/psibi/rego-mode

;; This file is not part of GNU Emacs.

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; A major mode for editing Rego file (See
;; https://www.openpolicyagent.org/docs/latest/policy-language/ to learn more) in Emacs.
;;
;; Some of its major features include:
;;
;;  - syntax highlighting (font lock),
;;
;;  - Basic indentation, multi line string support
;;
;;  - Automatic formatting on save (configurable)
;;
;;  - Error highlighting
;;
;;; Code:

(require 'ansi-color)
(require 'comint)
(require 'reformatter)

(defvar rego-mode-map
  (let ((map (make-sparse-keymap)))
    ;; (define-key map (kbd "C-c C-b") 'rego-repl-show)
    ;; (define-key map (kbd "C-c C-f") 'rego-format-buffer)
    ;; (define-key map (kbd "C-c C-t") 'rego-buffer-type)
    map)
  "Keymap for using `rego-mode'.")

(defgroup rego nil
  "Major mode for editing rego files"
  :group 'languages
  :prefix "rego-"
  :link '(url-link :tag "Site" "https://github.com/psibi/rego-mode")
  :link '(url-link :tag "Repository" "https://github.com/psibi/rego-mode"))

;; https://www.openpolicyagent.org/docs/latest/policy-reference/#grammar

;; define several category of keywords
(defvar rego-mode-keywords (regexp-opt '("as" "default" "else" "false" "import" "package" "not" "null" "true" "with") 'symbols))

(defvar rego-mode-types
  (regexp-opt '("String" "Number" "Char" "Array" "Object" "Set" "Boolean") 'symbols))

(defconst rego-mode-constants (regexp-opt '("true" "false") 'symbols))
(defconst rego-mode-numerals "\\_<[+\\-][1-9]+\\_>")
(defconst rego-mode-doubles "\\_<[+\\-]?[0-9]+\.[0-9]+\\_>")
(defconst rego-mode-operators (regexp-opt '("==" "!=" "<" ">" "<=" ">=" "+" "-" "*" "/" "&" "|")))
(defconst rego-mode-variables "\\([a-zA-Z][a-zA-Z0-9_]*\\)[[:space:]]*=")
(defconst rego-mode-variables2 "\\([a-zA-Z][a-zA-Z0-9_]*\\)[[:space:]]*:=")
(defconst rego-mode-rule-head "\\([a-zA-Z][a-zA-Z0-9_]*\\)[[:space:]]{")
(defconst rego-mode-expr-call "\\([a-zA-Z][a-zA-Z0-9_]*\\)(")

(defconst rego-mode-font-lock-keywords
  `( ;; Variables
    ;; (,rego-mode-urls . font-lock-function-name-face)
    (,rego-mode-expr-call . (1 font-lock-function-name-face))
    ;; (,rego-mode-types . font-lock-type-face)
    (,rego-mode-constants . font-lock-constant-face)
    (,rego-mode-operators . font-lock-builtin-face)
    (,rego-mode-variables . (1 font-lock-variable-name-face))
    (,rego-mode-variables2 . (1 font-lock-variable-name-face))
    (,rego-mode-rule-head . (1 font-lock-variable-name-face))
    (,rego-mode-keywords . font-lock-keyword-face)
    (,rego-mode-doubles . font-lock-constant-face)
    (,rego-mode-numerals . font-lock-constant-face)
    ))

;; Create the syntax table for this mode.
(defvar rego-mode-syntax-table
  (let ((st (make-syntax-table)))
    (modify-syntax-entry ?\[  "(]" st)
    (modify-syntax-entry ?\]  ")[" st)
    (modify-syntax-entry ?\( "()" st)
    (modify-syntax-entry ?\) ")(" st)
    (modify-syntax-entry ?\{  "(}1nb" st)
    (modify-syntax-entry ?\}  "){4nb" st)
    ;; End
    st)
  "Syntax table used while in `rego-mode'.")

;; Automatically use rego-mode for .rego files.
;;;###autoload
(add-to-list 'auto-mode-alist '("\\.rego\\'" . rego-mode))

;; The main mode functions
;;;###autoload
(define-derived-mode rego-mode prog-mode
  "Rego"
  "Major mode for editing Rego files."
  :group 'rego
  :keymap rego-mode-map
  :syntax-table rego-mode-syntax-table
  (setq font-lock-defaults '(rego-mode-font-lock-keywords))
  (setq-local indent-tabs-mode t)
  (setq-local tab-width 4)
  (setq-local comment-start "# ")
  (setq-local comment-end ""))

(defcustom rego-format-at-save t
  "If non-nil, the Rego buffers will be formatted after each save."
  :type 'boolean
  :group 'rego
  :safe 'booleanp)

(defcustom rego-opa-command "opa"
  "Command used to normalize Rego files.
Should be opa or the complete path to your opa executable,
  e.g.: /home/sibi/.local/bin/opa"
  :type 'file
  :group 'rego
  :safe 'stringp)

(reformatter-define rego-format
  :program rego-opa-command
  :args '("fmt")
  :group 'rego
  :lighter " OpaFmt")

(defcustom rego-format-arguments nil
  "Provide a list of arguments for the formatter e.g. '(\"--ascii\")."
  :type 'list
  :group 'rego
  :safe 'listp)

;; Provide ourselves:
(provide 'rego-mode)

;; Local Variables:
;; coding: utf-8
;; End:

;;; rego-mode.el ends here
