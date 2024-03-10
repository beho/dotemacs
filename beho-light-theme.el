(deftheme beho-light
  "A color theme based on the Oceanic.")

(custom-theme-set-faces
 'beho-light
 '(cursor ((t (:background "#14435D"))))
 '(fringe ((t (:background "#FFFFFF"))))
 '(region ((t (:background "#bfdffc"))))
 '(font-lock-builtin-face ((t (:foreground "#222222" :weight semi-bold))))
 '(font-lock-comment-face ((t (:foreground "#666666" :slant italic))))
 '(font-lock-function-name-face ((t (:weight bold))))
 '(font-lock-keyword-face ((t (:weight bold))))
 '(font-lock-string-face ((t (:foreground "#41663D")))) ; :slant italic
 '(font-lock-type-face ((t (:weight normal))))
 '(font-lock-constant-face ((t ())))
 '(font-lock-variable-name-face ((t ())))
 '(minibuffer-prompt ((t (:foreground "#14435D" :weight semi-bold))))
 '(font-lock-warning-face ((t (:foreground "#EC5f67" :weight normal))))
 ; disable italic
 ;; '(font-lock-comment-face ((t :italic nil)))
 '(highlight ((t (:background "#F5F5F5" :weight semi-bold))))
  ;; '(highlight ((t (:underline t))))
 ;; '(highlight ((t (:background "#FFFFFF"))))
 '(linum ((t (:foreground "#AB7967" :weight normal))))
 ;; '(line-number-current-line ((t (:foreground "#222222"))))
 '(line-number-current-line ((t (:background "#F5F5F5"))))
 '(mode-line ((t (:background "#E5E5E5" :foreground "#4F4F4F" :box "#BFBFBF" :weight semi-light :height 120))))
 '(mode-line-inactive ((t (:inherit 'mode-line :foreground "#6F6F6F"))))
 '(mode-line-highlight ((t (:inherit 'mode-line :background "#FF0000"))))
 ; paren
 '(show-paren-match ((t (:foreground "#222222" :background "#EAE72A" :weight bold))))
 '(show-paren-mismatch ((t (:background "#EC5F67"))))
 ; highlight sexp
 ;; '(hl-sexp-face ((t (:background "#edf6f9"))))
 '(default ((t (:background "#FFFFFF" :foreground "#222222" :width normal :weight normal))))) ; :background "#EFE5D5"

(provide-theme 'beho-light)
