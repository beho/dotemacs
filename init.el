;; -*- emacs-lisp -*- -*- lexical-binding: t; -*-

;;; utf-8 all the time
(setq locale-coding-system 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-selection-coding-system 'utf-8)
(prefer-coding-system 'utf-8)
;; (setq slime-net-coding-system 'utf-8-unix)

;; (setq shell-file-name "/usr/local/bin/fish")
;; (setq shell-file-name "/bin/bash")
;; for some reason this was set to /bin/bash but then path set in fish
;; was not picked up by exec-path-from-shell
(setq shell-file-name "/usr/bin/fish")
;; (setenv "SHELL" "/bin/bash")

(setq user-full-name "Svatopluk Šperka"
      user-mail-address "sperka@gmail.com")

;;disable Version Control
;; (setq vc-handled-backends nil)

;; warn when opening files bigger than 100MB
(setq large-file-warning-threshold 100000000)

;; quit Emacs directly even if there are running processes
(setq confirm-kill-processes nil)

;; the blinking cursor is nothing, but an annoyance
(blink-cursor-mode -1)
(setq cursor-type 'box)

;; disable the annoying bell ring
(setq ring-bell-function 'ignore)

;; enable y/n answers
(defalias 'yes-or-no-p 'y-or-n-p)

;; store all backup and autosave files in the tmp dir
(setq backup-directory-alist
      `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms
      `((".*" ,temporary-file-directory t)))

;; this is a single user workstation config, we don't need lockfiles
(setq create-lockfiles nil)

;; the tnoolbar is just a waste of valuable screen estate
;; in a tty tool-bar-mode does not properly auto-load, and is
;; already disabled anyway
(when (fboundp 'tool-bar-mode)
  (tool-bar-mode -1))

;; https://emacs.stackexchange.com/a/28746
(setq auto-window-vscroll nil)

(global-display-line-numbers-mode)
;; (global-hl-line-mode)

;; enable sRGB colors in the Cocoa version of emacs
(setq ns-use-srgb-colorspace t)

(setq
 ;; display line/column in modeline
 line-number-mode t
 column-number-mode t

 ;; various
 font-lock-maximum-decoration t
 visible-bell nil
 truncate-partial-width-windows nil)

;; show empty lines after buffer end
(set-default 'indicate-empty-lines t)

;; Typography
(set-frame-font
(concat
 ;; take the first installed font from this list, use at size 15
 (car (remove nil (mapcar (lambda (font) (car (member font (font-family-list))))
                          '("Berkeley Mono" "Victor Mono" "Menlo"))))
 "-10")
 nil
 t)

;;; font
;; (set-face-attribute 'default nil
;; 		    :family "Victor Mono" :height 140 :weight 'normal)
;; (set-face-attribute 'mode-line nil
;;                     :family "Victor Mono" :height 120 :weight 'normal)
;; (set-face-attribute 'mode-line-inactive nil
;;                     :family "Victor Mono" :height 120 :weight 'normal)
;; (set-face-attribute 'line-number-current-line nil
;;                     :foreground "#222222")


;; (set-face-italic 'font-lock-comment-face nil)

(setq-default line-spacing 1)

;; more useful frame title, that show either a file or a
;; buffer name (if the buffer isn't visiting a file)
(setq frame-title-format
      '((:eval (if (buffer-file-name)
                   (abbreviate-file-name (buffer-file-name))
                 "%b"))))

;; smart tab behavior - indent or complete
;; (setq tab-always-indent t)

;; Wrap lines at 120 characters
(setq-default fill-column 120)

;; Newline at end of file
(setq require-final-newline t)

;; delete the selection with a keypress
(delete-selection-mode t)

;; (global-prettify-symbols-mode 1)

;; (pixel-scroll-precision-mode t)

;; load modules
(mapc #'load-file (directory-files (concat user-emacs-directory "modules") t "[0-9]*.el$"))

;; remap lang modes to tree-sitter-ones
;; (setq major-mode-remap-alist
;;       '((ruby-mode . ruby-ts-mode)) ; https://github.com/tree-sitter/tree-sitter-ruby
;;       )


;;; tabs
; TODO styling
;; (global-tab-line-mode -1)
;; (menu-bar-mode 1)

;; (scroll-bar-mode -1)

(load-theme 'beho-light t)

(electric-pair-mode)


; parts stolen from https://github.com/bbatsov/emacs.d/blob/master/init.el





;; keep the installed packages in .emacs.d
;; (setq package-user-dir (expand-file-name "elpa" user-emacs-directory))






;; use common project files to detect project root
;; (use-package project-rootfile
;;   :config
;;   (add-to-list 'project-find-functions #'project-rootfile-try-detect)
;;   (add-to-list 'project-rootfile-list "build.zig"))


;; Window management experiments
;; https://www.masteringemacs.org/article/demystifying-emacs-window-manager

(setq switch-to-buffer-obey-display-actions t)


(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   '(consult-flycheck consult eglot envrc nix-mode lua-mode sideline-flycheck sideline jarchive markdown-mode dockerfile-mode json-mode json-reformat inf-ruby zig-mode cider clojure-mode flycheck-eglot flycheck yasnippet ace-window expand-region smartparens move-text which-key marginalia corfu orderless vertico avy visual-regexp undo-fu ag smart-tab paren-face doom-modeline nerd-icons magit exec-path-from-shell)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
