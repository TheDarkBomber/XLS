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
					(x-types '("dword"))
					(x-constants '())

					(x-keywords-regex (regexp-opt x-keywords 'words))
					(x-types-regex (regexp-opt x-types 'words))
					(x-constants-regex (regexp-opt x-constants 'words))
					(x-variables-regex "[A-Za-z_][A-Za-z0-9\-+/*:@=?!_]*")
					(x-functions-regex "\\([A-Za-z_][A-Za-z0-9\-+/*:@=?!_]*\\)\(")) ;; )

		`(
			 (,x-keywords-regex . font-lock-keyword-face)
			 (,x-types-regex . font-lock-type-face)
			 (,x-constants-regex . font-lock-constant-face)
			 (,x-functions-regex 1 font-lock-function-name-face)
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
