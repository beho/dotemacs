;; -*- emacs-lisp -*- -*- lexical-binding: t; -*-

;;; utf-8 all the time
(setq locale-coding-system 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-selection-coding-system 'utf-8)
(prefer-coding-system 'utf-8)
;; (setq slime-net-coding-system 'utf-8-unix)

(setq shell-file-name "/bin/bash")
;; (setenv "SHELL" "/bin/bash")

(setq user-full-name "Svatopluk Šperka"
      user-mail-address "sperka@gmail.com")

;;disable Version Control
(setq vc-handled-backends nil)

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
                           '("Victor Mono" "Menlo"))))
  "-12"))

;;; font
;; (set-face-attribute 'default nil
;; 		    :family "Victor Mono" :height 120 :weight 'normal)
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

(global-set-key (kbd "M-g f") 'avy-goto-line)

;; load modules
(mapc #'load-file (directory-files (concat user-emacs-directory "modules") t "[0-9]*.el$"))


;;; tabs
; TODO styling
;; (global-tab-line-mode -1)
;; (menu-bar-mode 1)

;; (scroll-bar-mode -1)

(load-theme 'beho-light t)


(use-package doom-themes
  :config
  (doom-themes-visual-bell-config))

(use-package doom-modeline
  :init
  (setq doom-themes-enable-bold t
        doom-themes-enable-italic t)
  :config
  (doom-modeline-mode t))


; parts stolen from https://github.com/bbatsov/emacs.d/blob/master/init.el





;; keep the installed packages in .emacs.d
;; (setq package-user-dir (expand-file-name "elpa" user-emacs-directory))






;; use common project files to detect project root
;; (use-package project-rootfile
;;   :config
;;   (add-to-list 'project-find-functions #'project-rootfile-try-detect)
;;   (add-to-list 'project-rootfile-list "build.zig"))



(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   '("a26c7fb9347b6b66fdad6cfe88fadeec395ddfb2ef13f80531c4e2f9cd083361" "f80e2e454abd167243b8bbbefa92d9e8a46813769ba0c49af8ff4582b943b8b4" "ee9f1c32046a8db565e21cd66b84e2ac6440ca3d633eea74194451ec57a8c846" "5c9bd73de767fa0d0ea71ee2f3ca6fe77261d931c3d4f7cca0734e2a3282f439" "37768a79b479684b0756dec7c0fc7652082910c37d8863c35b702db3f16000f8" "ea0e92e8625b7681a490123b40e45a6b7d88febcc4cd456c2f9ad27a9637eb2e" default))
 '(package-selected-packages
   '(paren-face vertico zig-mode which-key use-package treemacs-projectile treemacs-icons-dired subatomic-theme smooth-scrolling smartparens smart-tab slime selectrum-prescient ripgrep ponylang-mode paredit oceanic-theme nord-theme nim-mode move-text magit lsp-ui lsp-treemacs inf-ruby inf-clojure flycheck-pony expand-region exec-path-from-shell dockerfile-mode diminish cider bm atom-one-dark-theme))
 '(subatomic-more-visible-comment-delimiters t)
 '(warning-suppress-log-types '((comp)))
 '(warning-suppress-types 'nil))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(italic ((t (:slant normal)))))

;; '(smartparens bm smooth-scrolling-mode smooth-scrolling dockerfile-mode lsp-treemacs treemacs-icons-dired treemacs-projectile treemacs lsp-ui oceanic-theme inf-ruby slime exec-path-from-shell nim-mode ripgrep smart-tab ace-window diminish nord-theme subatomic-theme atom-one-dark-theme magit flycheck-pony ponylang-mode which-key selectrum-prescient selectrum-prscient selectrum inf-clojure clojure-inf flycheck lsp-mode paredit move-text rainbow-delimiters company projectile cider use-package)
