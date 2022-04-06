;; BSD 2-Clause License
;;
;; Copyright (c) 2022, TheDarkBomber
;; All rights reserved.
;;
;; Redistribution and use in source and binary forms, with or without
;; modification, are permitted provided that the following conditions are met:
;;
;; 1. Redistributions of source code must retain the above copyright notice, this
;;    list of conditions and the following disclaimer.
;;
;; 2. Redistributions in binary form must reproduce the above copyright notice,
;;   this list of conditions and the following disclaimer in the documentation
;;   and/or other materials provided with the distribution.
;;
;; THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
;; AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
;; IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
;; DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE
;; FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
;; DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
;; SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
;; CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY,
;; OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
;; OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

;;;###autoload
(defun xls-mode/xls-test ()
	"XLS Mode is present."
	(interactive)
	(message "XLS mode is present."))

(defvar xls-mode-syntax-table nil "Syntax table for XLS.")

(setq xls-mode-syntax-table
	(let ((x-syntax-table (make-syntax-table)))
		;; Uses standard // syntax for commenting
		(modify-syntax-entry ?\/ ". 12b" x-syntax-table)
		(modify-syntax-entry ?\n "> b" x-syntax-table)
		(modify-syntax-entry ?\' "\"" x-syntax-table)
		x-syntax-table))

(setq xls-mode/xls-highlight
	(let* (
					(x-keywords '(
												 "implement"
												 "impl"
												 "extern"
												 "if"
												 "else"
												 "operator"
												 "cdecl"
												 "fastcc"
												 "coldcc"
												 "tailcc"
												 "webkitjscc"
												 "win64cc"
												 "while"
												 "dowhile"
												 "label"
												 "jump"
												 "volatile"))
					(x-types '("dword" "word" "byte" "boole" "void"))
					(x-auxillary '("sizeof"))
					(x-constants '())

					(x-keywords-regex (regexp-opt x-keywords 'words))
					(x-types-regex (regexp-opt x-types 'words))
					(x-auxillary-regex (regexp-opt x-auxillary 'words))
					(x-constants-regex (regexp-opt x-constants 'words))
					(x-variables-regex "[A-Za-z_][A-Za-z0-9\-+/*:@=?!_]*")
					(x-pipef-regex "|>[[:space:]]\\([A-Za-z_][A-Za-z0-9\-+/*:@=?!_]*\\)")
					(x-functions-regex "\\([A-Za-z_][A-Za-z0-9\-+/*:@=?!_]*\\)\(")) ;; )

		`(
			 (,x-keywords-regex . font-lock-keyword-face)
			 (,x-types-regex . font-lock-type-face)
			 (,x-auxillary-regex . font-lock-builtin-face)
			 (,x-constants-regex . font-lock-constant-face)
			 (,x-functions-regex 1 font-lock-function-name-face)
			 (,x-pipef-regex 1 font-lock-function-name-face)
			 (,x-variables-regex . font-lock-variable-name-face)
			 )))

;;;###autoload
(define-derived-mode xls-mode prog-mode "XLS"
	"Major mode for editing XLS files"

	(setq font-lock-defaults '((xls-mode/xls-highlight)))
	(setq comment-start "//")
	(setq comment-end "")
	(set-syntax-table xls-mode-syntax-table))

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.xls\\'" . xls-mode))

(provide 'xls-mode)
