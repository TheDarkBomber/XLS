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
					(x-events '())
					(x-functions '())

					(x-keywords-regex (regexp-opt x-keywords 'words))
					(x-types-regex (regexp-opt x-types 'words))
					(x-constants-regex (regexp-opt x-constants 'words))
					(x-events-regex (regexp-opt x-events 'words))
					(x-functions-regex (regexp-opt x-functions 'words)))

		`(
			 (,x-keywords-regex . font-lock-type-face)
			 (,x-types-regex . font-lock-type-face)
			 (,x-constants-regex . font-lock-constant-face)
			 (,x-events-regex . font-lock-builtin-face)
			 (,x-functions-regex . font-lock-function-name-face)
			 )))

;;;###autoload
(define-derived-mode xls-mode prog-mode "XLS mode"
	"Major mode for editing XLS files"

	(setq font-lock-defaults '((xls-mode/xls-highlight)))
	(setq comment-start "//")
	(setq comment-end "")
	(set-syntax-table xls-mode-syntax-table))

(provide 'xls-mode)
