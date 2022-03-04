;;; mac keyboard mappings
(setq mac-option-modifier 'meta)
(setq mac-command-modifier 'hyper)

;;; tabs
; TODO styling
(global-tab-line-mode -1)
(menu-bar-mode -1)

(scroll-bar-mode -1)

;;; font
(set-face-attribute 'default nil
		    :family "Victor Mono" :height 120 :weight 'semibold)
(set-face-attribute 'mode-line nil
                    :family "Victor Mono" :height 120 :weight 'normal)
(set-face-attribute 'mode-line-inactive nil
                    :family "Victor Mono" :height 120 :weight 'normal)
(set-face-attribute 'line-number-current-line nil
                    :foreground "white smoke")

;;ligatures (emacs-mac)
;; (mac-auto-operator-composition-mode)

;;disable Version Control
(setq vc-handled-backends nil)

; parts stolen from https://github.com/bbatsov/emacs.d/blob/master/init.el

;;; package management
(require 'package)

(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/") t)

;; keep the installed packages in .emacs.d
(setq package-user-dir (expand-file-name "elpa" user-emacs-directory))

(package-initialize)
;; update the package metadata is the local cache is missing
(unless package-archive-contents
  (package-refresh-contents))

(setq user-full-name "Svatopluk Šperka"
      user-mail-address "sperka@gmail.com")

;; Always load newest byte code
(setq load-prefer-newer t)

;; reduce the frequency of garbage collection by making it happen on
;; each 50MB of allocated data (the default is on every 0.76MB)
(setq gc-cons-threshold 100000000)

;; Increase the amount of data which Emacs reads from the process.
(setq read-process-output-max (* 1024 1024)) ; 1mb

;; warn when opening files bigger than 100MB
(setq large-file-warning-threshold 100000000)

;; quit Emacs directly even if there are running processes
(setq confirm-kill-processes nil)

(defconst beho-savefile-dir (expand-file-name "savefile" user-emacs-directory))

;; create the savefile dir if it doesn't exist
(unless (file-exists-p beho-savefile-dir)
  (make-directory beho-savefile-dir))

;; the tnoolbar is just a waste of valuable screen estate
;; in a tty tool-bar-mode does not properly auto-load, and is
;; already disabled anyway
(when (fboundp 'tool-bar-mode)
  (tool-bar-mode -1))

;; the blinking cursor is nothing, but an annoyance
(blink-cursor-mode -1)

;; disable the annoying bell ring
(setq ring-bell-function 'ignore)

;; disable startup screen
(setq inhibit-startup-screen t)

(global-display-line-numbers-mode)

;; mode line settings
(line-number-mode t)
(column-number-mode t)
(size-indication-mode t)

;; enable y/n answers
(fset 'yes-or-no-p 'y-or-n-p)

;; more useful frame title, that show either a file or a
;; buffer name (if the buffer isn't visiting a file)
(setq frame-title-format
      '((:eval (if (buffer-file-name)
                   (abbreviate-file-name (buffer-file-name))
                 "%b"))))

;; Emacs modes typically provide a standard means to change the
;; indentation width -- eg. c-basic-offset: use that to adjust your
;; personal indentation width, while maintaining the style (and
;; meaning) of any files you load.
(setq-default indent-tabs-mode nil)   ;; don't use tabs to indent
(setq-default tab-width 8)            ;; but maintain correct appearance

;; smart tab behavior - indent or complete
(setq tab-always-indent 'complete)

;; Wrap lines at 120 characters
(setq-default fill-column 120)

;; Newline at end of file
(setq require-final-newline t)

;; delete the selection with a keypress
(delete-selection-mode t)

;; store all backup and autosave files in the tmp dir
(setq backup-directory-alist
      `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms
      `((".*" ,temporary-file-directory t)))

(prefer-coding-system 'utf-8)
(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)

(global-set-key (kbd "M-g f") 'avy-goto-line)

;;; packages
(unless (package-installed-p 'use-package)
  (package-install 'use-package))

(require 'use-package)
(setq use-package-verbose t)

; package for syncing Emacs with shell
(use-package exec-path-from-shell
  :ensure t)

; sync PATH, exec-path
(when (memq window-system '(mac ns x))
  (exec-path-from-shell-initialize))

(use-package smooth-scrolling
  :ensure t
  :config
  (setq smooth-scroll-margin 5)
  :init
  (smooth-scrolling-mode 1))

;; (setq scroll-step 10
;;       scroll-conservatively 1)

;; (setq scroll-step 10)

;; load diminish first so that we can use it further
(use-package diminish
  :ensure t)

(require 'diminish)

;;; built-in packages
(use-package paren
  :config
  (show-paren-mode +1))

;; TODO use smartparen and disable elec-pair and paredit
;; https://ebzzry.com/en/emacs-pairs/
;; https://github.com/Fuco1/smartparens
;; (use-package elec-pair
;;   :config
;;   (electric-pair-mode +1))

(use-package uniquify
  :config
  (setq uniquify-buffer-name-style 'forward)
  (setq uniquify-separator "/")
  ;; rename after killing uniquified
  (setq uniquify-after-kill-buffer-p t)
  ;; don't muck with special buffers
  (setq uniquify-ignore-buffers-re "^\\*"))

;; saveplace remembers your location in a file when saving files
(use-package saveplace
  :config
  (setq save-place-file (expand-file-name "saveplace" beho-savefile-dir))
  ;; activate it for all buffers
  (setq-default save-place t))

(use-package savehist
  :config
  (setq savehist-additional-variables
        ;; search entries
        '(search-ring regexp-search-ring)
        ;; save every minute
        savehist-autosave-interval 60
        ;; keep the home clean
        savehist-file (expand-file-name "savehist" beho-savefile-dir))
  (savehist-mode +1))

;; bookmarks
(use-package bm
  :ensure t
  :demand t

  :init
  ;; restore on load (even before you require bm)
  (setq bm-restore-repository-on-load t)

  :config
  (global-unset-key (kbd "C-x m"))

  (setq bm-highlight-style 'bm-highlight-only-fringe)
  (setq bm-marker 'bm-marker-left)
  
  ;; Allow cross-buffer 'next'
  (setq bm-cycle-all-buffers t)
  
  ;; where to store persistant files
  (setq bm-repository-file "~/.emacs.d/bm-repository")
  
  ;; save bookmarks
  (setq-default bm-buffer-persistence t)
  
  ;; Loading the repository from file when on start up.
  (add-hook 'after-init-hook 'bm-repository-load)
  
  ;; Saving bookmarks
  (add-hook 'kill-buffer-hook #'bm-buffer-save)
  
  ;; Saving the repository to file when on exit.
  ;; kill-buffer-hook is not called when Emacs is killed, so we
  ;; must save all bookmarks first.
  (add-hook 'kill-emacs-hook #'(lambda nil
                                 (bm-buffer-save-all)
                                 (bm-repository-save)))

  ;; the `after-save-hook' is not necessary to use to achieve persistence,
  ;; but it makes the bookmark data in repository more in sync with the file
  ;; state.
  (add-hook 'after-save-hook #'bm-buffer-save)

  ;; Restoring bookmarks
  (add-hook 'find-file-hook   #'bm-buffer-restore)
  (add-hook 'after-revert-hook #'bm-buffer-restore)
  
  ;; The `after-revert-hook' is not necessary to use to achieve persistence,
  ;; but it makes the bookmark data in repository more in sync with the file
  ;; state. This hook might cause trouble when using packages
  ;; that automatically reverts the buffer (like vc after a check-in).
  ;; This can easily be avoided if the package provides a hook that is
  ;; called before the buffer is reverted (like `vc-before-checkin-hook').
  ;; Then new bookmarks can be saved before the buffer is reverted.
  ;; Make sure bookmarks is saved before check-in (and revert-buffer)
  (add-hook 'vc-before-checkin-hook #'bm-buffer-save)

  :bind (("C-x m n" . bm-next)
         ("C-x m p" . bm-previous)
         ("C-x m m" . bm-toggle)))

;;; third-party packages

(use-package smart-tab
  :ensure t)

(use-package magit
  :ensure t)

(use-package ripgrep
  :ensure t)

;; (use-package nord-theme
;;   :ensure t
;;   :config
;;   (load-theme 'nord t))

(use-package oceanic-theme
  :ensure t
  :config
  (load-theme 'oceanic t))

(set-face-italic 'font-lock-comment-face nil)

;; (use-package atom-one-dark-theme
;;   :ensure t
;;   :config
;;   (load-theme 'atom-one-dark t))

;; (use-package subatomic-theme
;;   :ensure t
;;   :config
;;   (load-theme 'subatomic t))

;; (use-package zenburn-theme
;;   :ensure t
;;   :config
;;   (load-theme 'zenburn t))

(use-package selectrum
  :ensure t
  :config
  (selectrum-mode 1))

(use-package selectrum-prescient
  :ensure t
  :config
  (selectrum-prescient-mode 1))

(use-package which-key
  :ensure t
  :diminish 'which-key-mode
  :config
  (which-key-mode))

(use-package move-text
  :ensure t
  :bind
  (([(meta shift up)] . move-text-up)
   ([(meta shift down)] . move-text-down)))

(use-package rainbow-delimiters
  :ensure t)

;; (use-package paredit
;;   :ensure t
;;   :diminish 'paredit-mode)

(use-package smartparens
  :ensure t
  :bind (:map smartparens-mode-map
              ("C-M-a" . sp-beginning-of-sexp)
              ("C-M-e" . sp-end-of-sexp)

              ("C-<down>" . sp-down-sexp)
              ("C-<up>"   . sp-up-sexp)
              ("M-<down>" . sp-backward-down-sexp)
              ("M-<up>"   . sp-backward-up-sexp)

              ("C-M-f" . sp-forward-sexp)
              ("C-M-b" . sp-backward-sexp)

              ("C-M-n" . sp-next-sexp)
              ("C-M-p" . sp-previous-sexp)

              ("C-S-f" . sp-forward-symbol)
              ("C-S-b" . sp-backward-symbol)

              ("C-<right>" . sp-forward-slurp-sexp)
              ("M-<right>" . sp-forward-barf-sexp)
              ("C-<left>"  . sp-backward-slurp-sexp)
              ("M-<left>"  . sp-backward-barf-sexp)

              ("C-M-t" . sp-transpose-sexp)
              ("C-M-k" . sp-kill-sexp)
              ("C-k"   . sp-kill-hybrid-sexp)
              ("M-k"   . sp-backward-kill-sexp)
              ("C-M-w" . sp-copy-sexp)
              ("C-M-d" . delete-sexp)

              ("M-<backspace>" . backward-kill-word)
              ("C-<backspace>" . sp-backward-kill-word)
              ([remap sp-backward-kill-word] . backward-kill-word)

              ("M-[" . sp-backward-unwrap-sexp)
              ("M-]" . sp-unwrap-sexp)

              ("C-x C-t" . sp-transpose-hybrid-sexp)

              ("C-c ("  . wrap-with-parens)
              ("C-c ["  . wrap-with-brackets)
              ("C-c {"  . wrap-with-braces)
              ("C-c '"  . wrap-with-single-quotes)
              ("C-c \"" . wrap-with-double-quotes)
              ("C-c _"  . wrap-with-underscores)
              ("C-c `"  . wrap-with-back-quotes)))

(use-package expand-region
  :ensure t
  :bind ("C-=" . er/expand-region))

(use-package company
  :ensure t
  :diminish 'company-mode
  :config
  (setq company-idle-delay 0.1)
  (setq company-show-quick-access t)
  (setq company-tooltip-limit 10)
  (setq company-minimum-prefix-length 2)
  (setq company-tooltip-align-annotations t)
  ;; invert the navigation direction if the the completion popup-isearch-match
  ;; is displayed on top (happens near the bottom of windows)
  (setq company-tooltip-flip-when-above t)
;  (global-set-key (kbd "<M-tab>") 'company-complete) ; using smart-tab instead
  (global-company-mode)
  ;; (diminish 'company-mode)
  )

(use-package flycheck
  :ensure t
  :config
  (add-hook 'after-init-hook #'global-flycheck-mode))

(use-package projectile
  :ensure t
  :diminish ('projectile-mode . "p")
  :config
  (global-set-key (kbd "C-c p") 'projectile-command-map)
  (projectile-mode +1))

(use-package ace-window
  :ensure t
  :config
  (global-set-key (kbd "M-o") 'ace-window)
  (setq aw-dispatch-always t))

;; treemacs
(use-package treemacs
  :ensure t
  :defer t
  :init
  ;; (with-eval-after-load 'winum
  ;;   (define-key winum-keymap (kbd "M-0") #'treemacs-select-window))
  :config
  (progn
    (setq treemacs-collapse-dirs                   (if treemacs-python-executable 3 0)
          treemacs-deferred-git-apply-delay        0.5
          treemacs-directory-name-transformer      #'identity
          treemacs-display-in-side-window          t
          treemacs-eldoc-display                   t
          treemacs-file-event-delay                5000
          treemacs-file-extension-regex            treemacs-last-period-regex-value
          treemacs-file-follow-delay               0.2
          treemacs-file-name-transformer           #'identity
          treemacs-follow-after-init               t
          treemacs-expand-after-init               t
          treemacs-git-command-pipe                ""
          treemacs-goto-tag-strategy               'refetch-index
          treemacs-indentation                     2
          treemacs-indentation-string              " "
          treemacs-is-never-other-window           nil
          treemacs-max-git-entries                 5000
          treemacs-missing-project-action          'ask
          treemacs-move-forward-on-expand          nil
          treemacs-no-png-images                   nil
          treemacs-no-delete-other-windows         t
          treemacs-project-follow-cleanup          nil
          treemacs-persist-file                    (expand-file-name ".cache/treemacs-persist" user-emacs-directory)
          treemacs-position                        'left
          treemacs-read-string-input               'from-child-frame
          treemacs-recenter-distance               0.1
          treemacs-recenter-after-file-follow      nil
          treemacs-recenter-after-tag-follow       nil
          treemacs-recenter-after-project-jump     'always
          treemacs-recenter-after-project-expand   'on-distance
          treemacs-litter-directories              '("/node_modules" "/.venv" "/.cask")
          treemacs-show-cursor                     nil
          treemacs-show-hidden-files               t
          treemacs-silent-filewatch                nil
          treemacs-silent-refresh                  nil
          treemacs-sorting                         'alphabetic-asc
          treemacs-select-when-already-in-treemacs 'move-back
          treemacs-space-between-root-nodes        t
          treemacs-tag-follow-cleanup              t
          treemacs-tag-follow-delay                1.5
          treemacs-text-scale                      nil
          treemacs-user-mode-line-format           nil
          treemacs-user-header-line-format         nil
          treemacs-wide-toggle-width               70
          treemacs-width                           35
          treemacs-width-increment                 1
          treemacs-width-is-initially-locked       t
          treemacs-workspace-switch-cleanup        nil)

    ;; The default width and height of the icons is 22 pixels. If you are
    ;; using a Hi-DPI display, uncomment this to double the icon size.
    (treemacs-resize-icons 12)

    (treemacs-indent-guide-mode t)
    ;; (treemacs-follow-mode t)
    ;; (treemacs-filewatch-mode t)
    ;; (treemacs-fringe-indicator-mode 'always)

    ;; (pcase (cons (not (null (executable-find "git")))
    ;;              (not (null treemacs-python-executable)))
    ;;   (`(t . t)
    ;;    (treemacs-git-mode 'deferred))
    ;;   (`(t . _)
    ;;    (treemacs-git-mode 'simple)))

    (treemacs-hide-gitignored-files-mode nil))
  :bind
  (:map global-map
        ("M--"       . treemacs-select-window)
        ("C-x t 1"   . treemacs-delete-other-windows)
        ("C-x t t"   . treemacs)
        ("C-x t B"   . treemacs-bookmark)
        ("C-x t C-t" . treemacs-find-file)
        ("C-x t M-t" . treemacs-find-tag)))

(use-package treemacs-projectile
  :after (treemacs projectile)
  :ensure t)

(use-package treemacs-icons-dired
  :hook (dired-mode . treemacs-icons-dired-enable-once)
  :ensure t)

;; (use-package treemacs-magit
;;   :after (treemacs magit)
;;   :ensure t)

;; (use-package treemacs-persp ;;treemacs-perspective if you use perspective.el vs. persp-mode
;;   :after (treemacs persp-mode) ;;or perspective vs. persp-mode
;;   :ensure t
;;   :config (treemacs-set-scope-type 'Perspectives))
;; END treemacs

(use-package lsp-mode
  :ensure t
  :init
  (setq lsp-keymap-prefix "M-l"
        ; disable lsp autocomplete - use cider
        ;; works now
        ;lsp-completion-enable nil
        )
  :hook ((clojure-mode . lsp)
         (clojurec-mode . lsp)
         (clojurescript-mode . lsp)
         (zig-mode . lsp)
         (lsp-mode . lsp-enable-which-key-integration)
         (ruby-mode . lsp))
  :config
  ;; add paths to your local installation of project mgmt tools, like lein
  ;; (setenv "PATH" (concat
  ;;                  "/usr/local/bin" path-separator
  ;;                  (getenv "path")))
  (dolist (m '(clojure-mode
               clojurec-mode
               clojurescript-mode
               clojurex-mode))
    (add-to-list 'lsp-language-id-configuration `(,m . "clojure"))))

(use-package lsp-ui
  :ensure t
  :config
  (setq lsp-lens-enable nil
        lsp-ui-sideline-show-code-actions nil
        lsp-ui-doc-enable nil
        lsp-ui-peek-enable nil))

(use-package lsp-treemacs
  :ensure t)

;; Clojure

(use-package clojure-mode
  ; config rainbow-delimiters, paredit
  :ensure t
  :config
                                        ;  (add-hook 'clojure-mode-hook #'paredit-mode)
  (add-hook 'clojure-mode-hook #'smartparens-strict-mode)
  ; (add-hook 'clojure-mode-hook #'subword-mode)
  (add-hook 'clojure-mode-hook #'rainbow-delimiters-mode))

(use-package inf-clojure
    ; config rainbow-delimiters, paredit
  :ensure t)

(use-package cider
    ; config rainbow-delimiters, paredit
  :ensure t)

(defun disable-whitespace-mode ()
  (whitespace-mode -1))

;; Common Lisp
(use-package slime
  :ensure t
  :init
  (setq inferior-lisp-program "sbcl")
  :config
  (add-hook 'clojure-mode-hook #'smartparens-strict-mode)
  ; (add-hook 'clojure-mode-hook #'subword-mode)
  (add-hook 'clojure-mode-hook #'rainbow-delimiters-mode))

;; Zig

(use-package zig-mode
  :ensure t)

;; Pony

;; (use-package ponylang-mode
;;   :ensure t
;;   :init
;;   (setq ponylang-banner 1)
;;   :hook
;;   (ponylang-mode . disable-whitespace-mode)
;;   ;:bind-keymap
;;   ;k("<f6>" . ponylang-menu)
;;   )

;; (use-package flycheck-pony
;;   :ensure t)

;; Nim

;; (use-package nim-mode
;;   :ensure t
;;   :init
;;   (setq exec-path (cons "/Users/beho/.nimble/bin" exec-path))
;;   :hook
;;   (nim-mode . lsp))

;; turn off lockfiles as it causes errors with ponyc at the moment
;; (setq create-lockfiles n


;; Ruby

(use-package inf-ruby
  :ensure t
  :config
  (add-hook 'ruby-mode-hook #'inf-ruby-minor-mode))

(use-package ruby-mode
  :config
  (setq ruby-insert-encoding-magic-comment nil)
  (add-hook 'ruby-mode-hook #'subword-mode))

;; Dockerfile

(use-package dockerfile-mode
  :ensure t)

(diminish 'eldoc-mode)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   '("37768a79b479684b0756dec7c0fc7652082910c37d8863c35b702db3f16000f8" "ea0e92e8625b7681a490123b40e45a6b7d88febcc4cd456c2f9ad27a9637eb2e" default))
 '(package-selected-packages
   '(smartparens bm smooth-scrolling-mode smooth-scrolling dockerfile-mode lsp-treemacs treemacs-icons-dired treemacs-projectile treemacs lsp-ui oceanic-theme inf-ruby slime exec-path-from-shell nim-mode ripgrep smart-tab ace-window diminish nord-theme subatomic-theme atom-one-dark-theme magit flycheck-pony ponylang-mode which-key selectrum-prescient selectrum-prscient selectrum inf-clojure clojure-inf flycheck lsp-mode paredit move-text rainbow-delimiters company projectile cider use-package))
 '(subatomic-more-visible-comment-delimiters t)
 '(warning-suppress-types 'nil))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(italic ((t (:slant normal)))))

