;; -*- emacs-lisp -*- -*- lexical-binding: t; -*-

(setq package-archives
      '(("melpa"        . "https://melpa.org/packages/")
        ("melpa-stable" . "https://stable.melpa.org/packages/")
        ("nongnu"       . "https://elpa.nongnu.org/nongnu/")
        ("gnu"          . "https://elpa.gnu.org/packages/")))

(setq package-archive-priorities
      '(("melpa"        . 40)
        ("melpa-stable" . 30)
        ("nongnu"       . 20)
        ("gnu"          . 10)))

(package-initialize)
(setq package-install-upgrade-built-in t)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(if init-file-debug
    (setq use-package-verbose t
          use-package-expand-minimally nil
          use-package-compute-statistics t
          debug-on-error t)
  (setq use-package-verbose nil
        use-package-expand-minimally t))

(require 'use-package)
(setq use-package-always-ensure t)

; package for syncing Emacs with shell
(use-package exec-path-from-shell)

; sync PATH, exec-path
(when (memq window-system '(mac ns x))
  (exec-path-from-shell-initialize))

;; interacts with magit via seq dependency
(use-package magit
  :custom ((magit-diff-refine-hunk 'all)))

(use-package nerd-icons
  ;; install when not available
  :init
  (nerd-icons-install-fonts t)
  )

;; (use-package doom-themes
;;   :config
;;   (doom-themes-visual-bell-config))

(use-package doom-modeline
  :init
  ;; (setq doom-themes-enable-bold t
  ;;       doom-themes-enable-italic t)
  :config
  (doom-modeline-mode t))

;; eglot originating flymake errors do not get reported
;; https://gitlab.com/jessieh/mood-line/-/issues/28
;; (use-package mood-line
;;   :config
;;   (mood-line-mode)
;;   :custom
;;   (setq mood-line-glyph-alist mood-line-glyphs-unicode))


(use-package paren
  :config
  (show-paren-mode +1))

;; (setq show-paren-style 'parenthesis)

;; face dedicated to parens (dimming by default)
(use-package paren-face
  :config
  (global-paren-face-mode t))

(use-package highlight-sexp
  :load-path "packages/highlight-sexp/"
  ;; :hook ((lisp-mode . highlight-sexp-mode)
  ;;        (clojure-mode . highlight-sexp-mode))
  :init
  (add-hook 'lisp-mode-hook #'highlight-sexp-mode)
  (add-hook 'clojure-mode-hook #'highlight-sexp-mode)
  :config
  (setq hl-sexp-background-color "#F7F7F7"))

(use-package savehist
  :config
  (setq savehist-additional-variables
        ;; search entries
        '(search-ring regexp-search-ring)
        ;; save every minute
        savehist-autosave-interval 60
        ;; keep the home clean
        savehist-file (expand-file-name "savehist" user-emacs-directory))
  :init
  (savehist-mode))

(require 'uniquify)
(setq uniquify-buffer-name-style 'forward
      uniquify-separator "/"
      ;; rename after killing uniquified
      uniquify-after-kill-buffer-p t
      ;; don't muck with special buffers
      uniquify-ignore-buffers-re "^\\*")

(use-package smart-tab)



(use-package ag)

;; undo-tree-mode aliased to command+z/shift+command+z
(use-package undo-fu)

(use-package visual-regexp)

(use-package avy
  :config
  (global-set-key (kbd "M-g f") 'avy-goto-line))

;; TODO fix extensions removing straight
(use-package vertico
  ; :straight (:files (:defaults "extensions/*"))
  :init
  (vertico-mode))

(use-package orderless
  :init
  ;; Configure a custom style dispatcher (see the Consult wiki)
  ;; (setq orderless-style-dispatchers '(+orderless-dispatch)
  ;;       orderless-component-separator #'orderless-escapable-split-on-space)
  (setq completion-styles '(orderless basic)
        completion-category-defaults nil
        completion-category-overrides '((file (styles basic partial-completion)))))

;; TODO fix extensions removing straight
(use-package corfu
  ; :straight (:files (:defaults "extensions/*"))
  ;; :load-path "straight/build/corfu/extensions/"
  ;; Optional customizations
  :custom
  ;; (corfu-cycle t)                ;; Enable cycling for `corfu-next/previous'
  (corfu-auto t)                 ;; Enable auto completion
  (corfu-separator ?\s)          ;; Orderless field separator
  (corfu-quit-at-boundary nil)   ;; Never quit at completion boundary
  ;; (corfu-quit-no-match nil)      ;; Never quit, even if there is no match
  ;; (corfu-preview-current nil)    ;; Disable current candidate preview
  ;; (corfu-preselect 'prompt)      ;; Preselect the prompt
  ;; (corfu-on-exact-match nil)     ;; Configure handling of exact matches
  ;; (corfu-scroll-margin 5)        ;; Use scroll margin

  ;; Enable Corfu only for certain modes.
  ;; :hook ((prog-mode . corfu-mode)
  ;;        (shell-mode . corfu-mode)
  ;;        (eshell-mode . corfu-mode))

  ;; Recommended: Enable Corfu globally.
  ;; This is recommended since Dabbrev can be used globally (M-/).
  ;; See also `corfu-excluded-modes'.
  :init
  (global-corfu-mode)

  ;; triggers error in in post-command-hook (corfu--post-command)
  ;; (require 'corfu-echo)
  ;; (setq corfu-echo-delay t)
  ;; (corfu-echo-mode)
  
  ;; (require 'corfu-popupinfo)
  ;; (setq corfu-popupinfo-delay '(1.5 . 0))
    ;; (setq corfu-popupinfo-delay nil)
  ;; (corfu-popupinfo-mode)

  ;; (require 'corfu-indexed)
  )

;; Enable rich annotations using the Marginalia package
(use-package marginalia
  ;; Either bind `marginalia-cycle' globally or only in the minibuffer
  :bind (("M-A" . marginalia-cycle)
         :map minibuffer-local-map
         ("M-A" . marginalia-cycle))

  ;; The :init configuration is always executed (Not lazy!)
  :init

  ;; Must be in the :init section of use-package such that the mode gets
  ;; enabled right away. Note that this forces loading the package.
  (marginalia-mode))

(use-package which-key
  :diminish 'which-key-mode
  :config
  (which-key-mode))

(use-package move-text
  :bind
  (([(meta shift up)] . move-text-up)
   ([(meta shift down)] . move-text-down)))

(use-package smartparens
  :config
  ;; (sp-pair "'" nil :actions :rem)
  (sp-pair "`" nil :actions :rem)
  (sp-pair "'" nil :actions :rem)
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
              ("C-<left>" . sp-forward-barf-sexp)
              ("M-<left>"  . sp-backward-slurp-sexp)
              ("M-<right>"  . sp-backward-barf-sexp)

              ("C-M-t" . sp-transpose-sexp)
              ("C-M-k" . sp-kill-sexp)
              ("C-k"   . sp-kill-hybrid-sexp)
              ("M-k"   . sp-backward-kill-sexp)
              ("C-M-w" . sp-copy-sexp)
              
              ("M-<backspace>" . backward-kill-word)
              ("C-<backspace>" . sp-backward-kill-word)
              ([remap sp-backward-kill-word] . backward-kill-word)

              ;; ("M-[" . sp-backward-unwrap-sexp)
              ("M-s" . sp-unwrap-sexp)

              ;; ("C-x C-t" . sp-transpose-hybrid-sexp)

              ("C-c ("  . sp-wrap-round)
              ("C-c ["  . sp-wrap-square)
              ("C-c {"  . sp-wrap-curly)
              ;; ("C-c '"  . wrap-with-single-quotes)
              ;; ("C-c \"" . wrap-with-double-quotes)
              ;; ("C-c _"  . wrap-with-underscores)
              ;; ("C-c `"  . wrap-with-back-quotes)
              ))

(use-package expand-region
  :bind
  ("C-=" . er/expand-region)
  ("C-+" . er/contract-region))

(use-package ace-window
  :config
  (global-set-key (kbd "M-o") 'ace-window)
  (setq aw-dispatch-always t))

; required by lsp + company to offer snippets
(use-package yasnippet
  :config
  (yas-global-mode))

(use-package asdf
  :load-path "packages/asdf.el/"
  :config
  (setq asdf-binary "/usr/local/bin/asdf")
  ;; (asdf-enable)
  )

(use-package flymake)
;; (use-package flymake-ruby)

;; (use-package aggressive-indent)

;; do not use built-in flymake because lsp-mode-ui cannot display errors in sideline
;; https://github.com/emacs-lsp/lsp-ui/issues/210
;; (use-package flycheck
;;   :config
;;   (add-hook 'after-init-hook #'global-flycheck-mode))

;; (use-package projectile
;;   :diminish 'projectile-mode ;('projectile-mode . "P")
;; ;  :diminish ('projectile-mode . "PPP")
;;   :config
;;   (global-set-key (kbd "C-c p") 'projectile-command-map)
;;   (projectile-mode +1)
;;   (setq projectile-create-missing-test-files t))

;; treemacs
;; (use-package treemacs
;;   :defer t
;;   :init
;;   ;; (with-eval-after-load 'winum
;;   ;;   (define-key winum-keymap (kbd "M-0") #'treemacs-select-window))
;;   :config
;;   (progn
;;     (setq treemacs-collapse-dirs                   (if treemacs-python-executable 3 0)
;;           treemacs-deferred-git-apply-delay        0.5
;;           treemacs-directory-name-transformer      #'identity
;;           treemacs-display-in-side-window          t
;;           treemacs-eldoc-display                   t
;;           treemacs-file-event-delay                5000
;;           treemacs-file-extension-regex            treemacs-last-period-regex-value
;;           treemacs-file-follow-delay               0.2
;;           treemacs-file-name-transformer           #'identity
;;           treemacs-follow-after-init               t
;;           treemacs-expand-after-init               t
;;           treemacs-git-command-pipe                ""
;;           treemacs-goto-tag-strategy               'refetch-index
;;           treemacs-indentation                     2
;;           treemacs-indentation-string              " "
;;           treemacs-is-never-other-window           nil
;;           treemacs-max-git-entries                 5000
;;           treemacs-missing-project-action          'ask
;;           treemacs-move-forward-on-expand          nil
;;           treemacs-no-png-images                   nil
;;           treemacs-no-delete-other-windows         t
;;           treemacs-project-follow-cleanup          nil
;;           treemacs-persist-file                    (expand-file-name ".cache/treemacs-persist" user-emacs-directory)
;;           treemacs-position                        'left
;;           treemacs-read-string-input               'from-child-frame
;;           treemacs-recenter-distance               0.1
;;           treemacs-recenter-after-file-follow      nil
;;           treemacs-recenter-after-tag-follow       nil
;;           treemacs-recenter-after-project-jump     'always
;;           treemacs-recenter-after-project-expand   'on-distance
;;           treemacs-litter-directories              '("/node_modules" "/.venv" "/.cask")
;;           treemacs-show-cursor                     nil
;;           treemacs-show-hidden-files               t
;;           treemacs-silent-filewatch                nil
;;           treemacs-silent-refresh                  nil
;;           treemacs-sorting                         'alphabetic-asc
;;           treemacs-select-when-already-in-treemacs 'move-back
;;           treemacs-space-between-root-nodes        t
;;           treemacs-tag-follow-cleanup              t
;;           treemacs-tag-follow-delay                1.5
;;           treemacs-text-scale                      nil
;;           treemacs-user-mode-line-format           nil
;;           treemacs-user-header-line-format         nil
;;           treemacs-wide-toggle-width               70
;;           treemacs-width                           35
;;           treemacs-width-increment                 1
;;           treemacs-width-is-initially-locked       t
;;           treemacs-workspace-switch-cleanup        nil)

;;     ;; The default width and height of the icons is 22 pixels. If you are
;;     ;; using a Hi-DPI display, uncomment this to double the icon size.
;;     (treemacs-resize-icons 12)

;;     (treemacs-indent-guide-mode t)
;;     ;; (treemacs-follow-mode t)
;;     ;; (treemacs-filewatch-mode t)
;;     ;; (treemacs-fringe-indicator-mode 'always)

;;     ;; (pcase (cons (not (null (executable-find "git")))
;;     ;;              (not (null treemacs-python-executable)))
;;     ;;   (`(t . t)
;;     ;;    (treemacs-git-mode 'deferred))
;;     ;;   (`(t . _)
;;     ;;    (treemacs-git-mode 'simple)))

;;     (treemacs-hide-gitignored-files-mode nil))
;;   :bind
;;   (:map global-map
;;         ("M--"       . treemacs-select-window)
;;         ("C-x t 1"   . treemacs-delete-other-windows)
;;         ("C-x t t"   . treemacs)
;;         ("C-x t B"   . treemacs-bookmark)
;;         ("C-x t C-t" . treemacs-find-file)
;;         ("C-x t M-t" . treemacs-find-tag)))

;; (use-package treemacs-projectile
;;   :after (treemacs projectile))

;; (use-package treemacs-icons-dired
;;   :hook (dired-mode . treemacs-icons-dired-enable-once))

;; (use-package posframe)
;; display flymake diagnostics at point
;; (require 'flymake-posframe)
;; (add-hook 'flymake-mode 'flymake-posframe-mode)

;; (use-package flymake-posframe
;;   :straight (:files (:defaults "extensions/*"))
;;   ;; :load-path "downloaded/flymake-posframe"
;;   :hook (flymake-mode . flymake-posframe-mode))

;; Clojure

(use-package clojure-mode
  ; config rainbow-delimiters, paredit
  :config
  (add-hook 'clojure-mode-hook #'smartparens-strict-mode)
  (add-hook 'clojure-mode-hook #'subword-mode)
  ;; (add-hook 'clojure-mode-hook #'eglot-ensure)
  )

;; (use-package clojure-ts-mode
;;   :config
;;   (add-hook 'clojure-mode-hook #'smartparens-strict-mode)
  ;; (add-hook 'clojure-mode-hook #'subword-mode)
;;   )

;; (use-package inf-clojure) ; config rainbow-delimiters, paredit

(use-package cider
  :config
  (setq cider-show-error-buffer t ;'only-in-repl
        cider-font-lock-dynamically t ; use lsp semantic tokens
        cider-eldoc-display-for-symbol-at-point nil ; use lsp
        cider-prompt-for-symbol nil
        cider-use-xref nil) ; use lsp
  ;; use lsp completion
  (add-hook 'cider-mode-hook (lambda () (remove-hook 'completion-at-point-functions #'cider-complete-at-point))
;  (remove-hook 'completion-at-point-functions #'cider-complete-at-point t)
 ; (add-hook 'completion-at-point-functions #'cider-complete-at-point 0 t)
  ))       ; config rainbow-delimiters, paredit

(defun my/remove-eglot-documentation-functions ()
      (interactive)
      (when (and (bound-and-true-p cider-mode) (cider-connected-p))
        (remove-hook 'eldoc-documentation-functions #'eglot-hover-eldoc-function t)
        (remove-hook 'eldoc-documentation-functions #'eglot-signature-eldoc-function t)
        (remove-hook 'eldoc-documentation-functions #'eglot-completion-at-point t)
        (setq completion-at-point-functions (remove 'eglot-completion-at-point completion-at-point-functions))))

;; hook ((cider-connected . my/remove-eglot-documentation-functions))

(defun disable-whitespace-mode ()
  (whitespace-mode -1))

;; Common Lisp
;; (use-package slime
;;   :init
;;   (setq inferior-lisp-program "sbcl")
;;   :config
;;   (add-hook 'slime-mode-hook #'smartparens-strict-mode))

;; neo4j cypher
;; (use-package cypher-mode)

;; Zig

(use-package zig-mode)

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
  :config
  (add-hook 'ruby-mode-hook #'inf-ruby-minor-mode))

(use-package ruby-mode
  :config
  (setq ruby-insert-encoding-magic-comment nil)
  (add-hook 'ruby-mode-hook #'subword-mode))

;; JS

;; (use-package js2-mode
;;   :config
;;   (setq-default js2-auto-indent-p t)
;;   (setq-default js2-global-externs '("module" "require" "jQuery" "$" "_" "buster" "sinon" "assert" "refute" "setTimeout" "clearTimeout" "setInterval" "clearInterval" "location" "__dirname" "console" "JSON"))
;;   (add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode)))
; (use-package js-refactor)

(use-package json-reformat
  :config
  (setq json-reformat:indent-width 2))

(use-package json-mode)

;; Dockerfile

(use-package dockerfile-mode)

;; Markdown

(use-package markdown-mode)

;;; eglot

;; (use-package consult-eglot)

(use-package eglot
  :commands (eglot eglot-ensure)
  :custom-face (eglot-highlight-symbol-face ((t (:inherit 'highlight))))
  :hook (
         ;; (clojure-mode . eglot-ensure)
         ;; (clojurec-mode . eglot-ensure)
         ;; (clojurescript-mode . eglot-ensure)
         (before-save . eglot-format-buffer))
  :bind (:map eglot-mode-map
              ("C-." . eglot-code-actions)
              ("M-l r r" . eglot-rename)
              ("M-l = =" . eglot-format))
  :config
  ;; (add-hook 'eglot-managed-mode-hook #'eldoc-box-hover-at-point-mode t)
  ;; configure ruby-lsp as server for ruby
  (add-to-list 'eglot-server-programs '((ruby-mode ruby-ts-mode) "ruby-lsp"))
  (setq eglot-autoshutdown t
        eglot-confirm-server-initiated-edits nil
        eglot-extend-to-xref t
        eglot-connect-timeout 60) ; clojure-lsp sometimes takes longer to start
  )

;; peek into JARs
(use-package jarchive
  ;; :straight (jarchive :type git
  ;;                     :host nil
  ;;                     :repo "https://git.sr.ht/~dannyfreeman/jarchive")
  :hook ((clojure-mode . jarchive-setup)
         (clojurec-mode . jarchive-setup)))

;; (use-package eldoc
;;   :config
;;   (setq eldoc-echo-area-use-multiline-p nil))

;; ;; (use-package flymake
;; ;;    ;; :config
;; ;;    ;; (setq eldoc-documentation-function 'eldoc-documentation-compose)
;; ;;    ;; (add-hook 'flymake-mode-hook
;; ;;    ;;           (lambda ()
;; ;;    ;;             (setq eldoc-documentation-functions
;; ;;    ;;                   (cons 'flymake-eldoc-function
;; ;;    ;;                         (delq 'flymake-eldoc-function eldoc-documentation-functions)))))
;; ;;    )

;; (use-package flymake-diagnostic-at-point
;;   :after flymake
;;   :config
;;   (add-hook 'flymake-mode-hook #'flymake-diagnostic-at-point-mode))

;; (use-package lsp-mode
;;   :init
;;   (setq lsp-keymap-prefix "M-l"
;;         ; disable lsp autocomplete - use cider
;;         ;; works now
;;         ;lsp-completion-enable nil
;;         )
;;   :hook ((clojure-mode . lsp)
;;          (clojurec-mode . lsp)
;;          (clojurescript-mode . lsp)
;;          ;; (zig-mode . lsp)
;;          (lsp-mode . lsp-enable-which-key-integration)
;;          ;; (ruby-mode . lsp)
;;          )
;;   :config
;;   ;; add paths to your local installation of project mgmt tools, like lein
;;   ;; (setenv "PATH" (concat
;;   ;;                  "/usr/local/bin" path-separator
;;   ;;                  (getenv "path")))
;;   ;; (dolist (m '(clojure-mode
;;   ;;              clojurec-mode
;;   ;;              clojurescript-mode
;;   ;;              clojurex-mode))
;;   ;;   (add-to-list 'lsp-language-id-configuration `(,m . "clojure")))
;;   )

;; TODO customize font for "warnings" (clj: Unused public var ...)
;; (use-package lsp-ui
;;   :config
;;   (setq lsp-lens-enable nil
;;         lsp-ui-sideline-show-code-actions nil
;;         lsp-ui-sideline-show-diagnostics t
;;         lsp-ui-doc-enable nil
;;         lsp-ui-peek-enable nil))

;; (use-package lsp-treemacs)

(use-package sideline
  :hook
  ;; (flycheck-mode . sideline-mode)
  (flymake-mode . sideline-mode)
  :init
  (setq sideline-backends-right '(sideline-flymake)
        sideline-backends-skip-current-line t  ; don't display on current line
        sideline-order-left 'down              ; or 'up
        sideline-order-right 'up               ; or 'down
        sideline-format-left "%s   "           ; format for left aligment
        sideline-format-right "   %s"          ; format for right aligment
        sideline-priority 100                  ; overlays' priority
        sideline-display-backend-name t        ; display the backend name

        sideline-flymake-display-mode 'line))

(use-package sideline-flymake)

;; (use-package sideline-flycheck
;;   :hook (flycheck-mode . sideline-flycheck-setup))

;; a few more useful configurations...
(use-package emacs
  :init
  ;; tab cycle if there are only few candidates
  (setq completion-cycle-threshold 3)

  ;; Emacs 28: Hide commands in M-x which do not apply to the current mode.
  ;; Corfu commands are hidden, since they are not supposed to be used via M-x.
  ;; (setq read-extended-command-predicate
  ;;       #'command-completion-default-include-p)

  ;; Enable indentation+completion using the TAB key.
  ;; `completion-at-point' is often bound to M-TAB.
  (setq tab-always-indent 'complete))
