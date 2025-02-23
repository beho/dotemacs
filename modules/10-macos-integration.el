;; -*- emacs-lisp -*-

;; use OS X's Spotlight for M-x locate
(setq locate-make-command-line (lambda (s) `("mdfind" "-name" ,s)))

;; alt-click for mouse-2, command-click for mouse-3
;; ...is this broken?
(setq mac-emulate-three-button-mouse t)

(setq shift-select-mode t)
(delete-selection-mode t)

;;;; Normalize key bindings with Mac OS X system one
(global-set-key (kbd "<s-up>")    'beginning-of-buffer)
(global-set-key (kbd "<s-down>")  'end-of-buffer)
(global-set-key (kbd "<s-left>")  'move-beginning-of-line)
(global-set-key (kbd "<s-right>") 'move-end-of-line)

(define-key global-map (kbd "s-+") 'text-scale-increase)
(define-key global-map (kbd "s--") 'text-scale-decrease)

;; fn+option+delete = kill word to the right in OS X inputs
(define-key global-map (kbd "<M-kp-delete>") 'paredit-forward-kill-word)

;; undo-tree-mode aliased to command+z/shift+command+z
(global-set-key (kbd "s-z") 'undo-fu-only-undo)
(global-set-key (kbd "s-Z") 'undo-fu-only-redo)

;; command-f, the default OSX search keybinding, but with regexp
(global-set-key (kbd "s-f") 'isearch-forward-regexp)
(global-set-key (kbd "s-g") 'isearch-repeat-forward)

;; command-f, the default OSX search keybinding, but with regexp
(global-set-key (kbd "s-f") 'isearch-forward-regexp)
(global-set-key (kbd "s-g") 'isearch-repeat-forward)

;; make M-up and M-down the same as C-up and C-down because the former
;; is how it's bound in OSX
(global-set-key (kbd "<M-up>") 'backward-paragraph)
(global-set-key (kbd "<M-down>") 'forward-paragraph)

;; option-delete = backword-kill-word in OS X
(global-set-key (kbd "M-<backspace>") 'backward-kill-word)

;; I like a single frame, so I'd rather have this kill the buffer
(global-set-key (kbd "s-w") 'kill-this-buffer)

;; I never want to see the OS file finder
;; (require 'find-file-in-project)
;; (global-set-key (kbd "s-o") 'find-file-in-project-by-selected)

;; but why would you ever quit emacs?
(global-set-key (kbd "s-q") 'save-buffers-kill-emacs)

;; the ESC key behavior everyone expects
(global-set-key (kbd "<escape>") 'keyboard-quit)

;; In dired, move deletions to trash
(setq delete-by-moving-to-trash t)

;; don't use the right alt/option key as meta, so it can still be used
;; to type accented characters. Fück yeah.
(setq ns-right-alternate-modifier nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; YAMAMOTO MITSUHARU (AKA RAILWAY CAT) EMACS

(when (string-match "AppKit" (version))

  ;; emacs-mac has its own precision mode os turn off 29's one
  ;; (pixel-scroll-precision-mode nil)

  ;; default railway has these the other way round
  (setq mac-option-modifier 'meta)
  (setq mac-command-modifier 'super)
  ;; allow right opt for char insert
  (setq mac-right-option-modifier 'none)

  ;; and doesn't set the default CUA bindings
  (global-set-key [(super a)] 'mark-whole-buffer)
  (global-set-key [(super c)] 'kill-ring-save)
  (global-set-key [(super v)] 'yank)
  (global-set-key [(super x)] 'kill-region)
  (global-set-key [(super s)] 'save-buffer)
  (global-set-key [(super l)] 'goto-line)
  (global-set-key [(super w)] 'kill-this-buffer)
  (global-set-key [(super z)] 'undo)

  ;; turn off super disturbing visible bell
  (setq visible-bell nil)

  (when (fboundp 'mac-auto-operator-composition-mode)
    ;; ligatures
    (mac-auto-operator-composition-mode)))

