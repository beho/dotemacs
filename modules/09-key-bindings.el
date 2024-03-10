;; -*- emacs-lisp -*- -*- lexical-binding: t; -*-

;; format buffer
(defun indent-buffer ()
  (interactive)
  (save-excursion
    (indent-region (point-min) (point-max))))

(keymap-global-set "C-x C-\\" 'indent-buffer)


;; editing commands
;; (global-unset-key (kbd "C-w"))
(keymap-global-set "C-`" 'backward-kill-word)
(keymap-global-set "C-M-g" 'duplicate-line)

;; navigate flymake errors
;; (define-key flymake-mode-map (kbd "M-n") 'flymake-goto-next-error)
;; (define-key flymake-mode-map (kbd "M-p") 'flymake-goto-prev-error)
;; (define-key flymake-mode-map (kbd "C-x p -") 'flymake-show-project-diagnostics)

;; navigate windows
(keymap-global-set "M-s-<left>"  'windmove-left)
(keymap-global-set "M-s-<right>" 'windmove-right)
(keymap-global-set "M-s-<up>"    'windmove-up)
(keymap-global-set "M-s-<down>"  'windmove-down)

;; use calva's binding for eval
(keymap-global-set "C-<return>" 'eval-last-sexp)
(define-key cider-mode-map "C-<return>" 'cider-eval-last-sexp)
;; (define-key cider-mode-map (kbd "C-<return>") 'cider-eval-)

