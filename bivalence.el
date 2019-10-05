;;(setq bi-keywords '("->" "@", ("[A-Z]") . (font-lock-constant-face)))
;;		    ""))

(setq bi-keywords '("->" "^@" ":"
		    ("\\b[A-Z]+[A-Z|a-z]*\\b" 0 font-lock-constant-face t)
		    ("\\b[0-9]+\\b" 0 font-lock-constant-face t)
		    ("\\b'[^']*'\\b" 0 font-lock-constant-face t)))
;;|

(define-minor-mode bi-mode
  "Minor mode for bivalence programming."
  :lighter " bivalence"
  (font-lock-add-keywords nil bi-keywords)

  (if (fboundp 'font-lock-flush)
      (font-lock-flush)
    (when font-lock-mode
      (with-no-warnings (font-lock-fontify-buffer)))))
