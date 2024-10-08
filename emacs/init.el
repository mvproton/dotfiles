;;; init.el --- Main configuration file -*- lexical-binding: t; no-byte-compile: t -*-

(setq debug-on-error t)

(use-package use-package
  :no-require
  :custom
  (use-package-enable-imenu-support t))

(use-package early-init
  :no-require
  :unless (featurep 'early-init)
  :config
  (load-file (locate-user-emacs-file "early-init.el")))

(use-package delight
  :ensure t)

(use-package gcmh
  :ensure t
  :preface
  :hook (after-init . gcmh-mode)
  :delight gcmh-mode)

(use-package startup
  :no-require
  :custom
  (user-mail-address "mvproton@gmail.com")
  (user-full-name "Maks"))

(use-package subr
  :no-require t
  :init
  (fset 'yes-or-no-p 'y-or-n-p))

(use-package files
  :preface
  (defvar backup-dir
    (expand-file-name ".cache/backups" user-emacs-directory)
    "Directory to store backups.")
  (defvar auto-save-dir
    (expand-file-name ".cache/auto-save/" user-emacs-directory)
    "Directory to store auto-save files.")
  :bind ("C-x C-S-f" . sudo-find-file)
  :hook (before-save . delete-trailing-whitespace)
  :custom
  (backup-by-copying t)
  (create-lockfiles nil)
  (auto-save-no-message nil)
  (backup-directory-alist `(("." . ,(expand-file-name ".cache/backups" user-emacs-directory))))
  (auto-save-file-name-transforms `((".*" ,(expand-file-name ".cache/auto-save/" user-emacs-directory) t)))
  (require-final-newline t)
  (auto-save-interval 100)
  :config
  (unless (file-exists-p auto-save-dir)
    (make-directory auto-save-dir t)))

(use-package cus-edit
  :custom
  (custom-file (expand-file-name "custom.el" user-emacs-directory))
  :init
  (load custom-file :noerror))

(use-package functions
  :defer t
  :preface
  (defun sudo-find-file (file-name)
    (interactive "Fsudo find-file: ")
    (let ((tramp-file-name (concat "/sudo::" (expand-file-name file-name))))
      (find-file tramp-file-name)))
  (defun edit-init-file ()
    (interactive)
    (find-file (expand-file-name "init.el" user-emacs-directory)))
  (defun edit-early-init-file ()
    (interactive)
    (find-file (expand-file-name "early-init.el" user-emacs-directory)))
  (defun copy-backward-sexp ()
    (interactive)
    (let* ((sexp-end (point))
           (_ (backward-sexp))
           (sexp-begin (point))
           (sexp (buffer-substring-no-properties sexp-begin sexp-end)))
      (goto-char sexp-end)
      sexp))
  (defun disable-themes ()
    (dolist (theme custom-enabled-themes)
      (disable-theme theme)
      (message "Theme [%s] disabled" theme)))
  (defun install-treesit-plugins ()
    (let ((treesit-plugins-dir (concat user-emacs-directory "tree-sitter")))
      (unless (file-exists-p treesit-plugins-dir)
        (make-directory treesit-plugins-dir))
      (dolist (plugin (mapcar #'car treesit-language-source-alist))
        (let* ((plugin-file-name (concat "libtree-sitter-" (symbol-name plugin) ".so"))
               (plugin-path (concat treesit-plugins-dir "/" plugin-file-name)))
          (unless (file-exists-p plugin-path)
            (treesit-install-language-grammar plugin)
            (message "Tree-sitter plugin [%s] installed." plugin))))))
  (provide 'functions))

(use-package emacs
  :init
  (setq-default indent-tabs-mode nil
                tab-width 4
                fill-column 100
                debug-on-error nil
                truncate-lines nil
                inhibit-startup-screen t
                ring-bell-function 'ignore
                eval-expression-print-level nil
                eval-expression-print-length nil
                hscroll-step 1
                hscroll-margin 0
                warning-minimum-level :error
                initial-scratch-message ";;; Scratch buffer\n\n"
                default-input-method "russian-computer"
                frame-title-format
                (list (format "%s@%s %%S: %%j " user-login-name (system-name))
                      '(buffer-file-name "%f" (dired-directory dired-directory "%b"))))
  (setq ring-bell-function 'ignore
        enable-recursive-minibuffers t
        treesit-language-source-alist
        '((json "https://github.com/tree-sitter/tree-sitter-json")
          (javascript "https://github.com/tree-sitter/tree-sitter-javascript" "master" "src")
          (typescript "https://github.com/tree-sitter/tree-sitter-typescript" "master" "typescript/src")
          (tsx "https://github.com/tree-sitter/tree-sitter-typescript" "master" "tsx/src")
          (rust "https://github.com/tree-sitter/tree-sitter-rust" "master" "src")
          (php "https://github.com/tree-sitter/tree-sitter-php" "master" "php/src")))
  (put 'downcase-region 'disabled nil)
  (put 'upcase-region 'disabled nil)
  :bind (("M-g a" . "α")
         ("M-g b" . "β")
         ("M-g g" . "γ")
         ("M-g d" . "δ")
         ("M-g e" . "ε")
         ("M-g z" . "ζ")
         ("M-g h" . "η")
         ("M-g q" . "θ")
         ("M-g i" . "ι")
         ("M-g k" . "κ")
         ("M-g l" . "λ")
         ("M-g m" . "μ")
         ("M-g n" . "ν")
         ("M-g x" . "ξ")
         ("M-g o" . "ο")
         ("M-g p" . "π")
         ("M-g r" . "ρ")
         ("M-g s" . "σ")
         ("M-g t" . "τ")
         ("M-g u" . "υ")
         ("M-g f" . "ϕ")
         ("M-g j" . "φ")
         ("M-g c" . "χ")
         ("M-g y" . "ψ")
         ("M-g w" . "ω")
         ("M-g A" . "Α")
         ("M-g B" . "Β")
         ("M-g G" . "Γ")
         ("M-g D" . "Δ")
         ("M-g E" . "Ε")
         ("M-g Z" . "Ζ")
         ("M-g H" . "Η")
         ("M-g Q" . "Θ")
         ("M-g I" . "Ι")
         ("M-g K" . "Κ")
         ("M-g L" . "Λ")
         ("M-g M" . "Μ")
         ("M-g N" . "Ν")
         ("M-g X" . "Ξ")
         ("M-g O" . "Ο")
         ("M-g P" . "Π")
         ("M-g R" . "Ρ")
         ("M-g S" . "Σ")
         ("M-g T" . "Τ")
         ("M-g U" . "Υ")
         ("M-g F" . "Φ")
         ("M-g J" . "Φ")
         ("M-g C" . "Χ")
         ("M-g Y" . "Ψ")
         ("M-g W" . "Ω"))
  :config (install-treesit-plugins))

(use-package window
  :bind (("M-o" . other-window)
         ("M-p" . scroll-up-n-lines)
         ("M-n" . scroll-down-n-lines))
  :config
  (defun scroll-up-n-lines ()
    (interactive)
    (scroll-up 3))
  (defun scroll-down-n-lines ()
    (interactive)
    (scroll-down 3)))

(when window-system
  (use-package modus-themes
    :vc ( :url "https://github.com/protesilaos/modus-themes"
          :branch "main"
          :rev :newest)
    :custom
    (modus-themes-org-blocks nil)
    (modus-themes-syntax '(faint alt-syntax))
    (modus-themes-region '(bg-only no-extend))
    (modus-themes-completions '((matches . (intense bold))
                                (selection . (intense))))
    (modus-operandi-palette-overrides '((bg-main "#fbfbfb")
                                        (string "#702f00")
                                        (bg-paren-match "#d2d2d2")))
    (modus-vivendi-palette-overrides '((bg-main "#181818")))
    (modus-themes-mode-line '(borderless))
    (modus-themes-fringes nil)
    :init
    ;; (disable-themes)
    (load-theme 'modus-operandi t)
    :config
    (set-face-attribute 'default nil :font "Fira Code" :height
                        (if (string-match-p (regexp-quote "laptop")
                                            (system-name))
                            120
                          113))
    (set-face-attribute 'show-paren-match nil :weight 'extra-bold)
    :bind ("<f5>" . modus-themes-toggle)))

(use-package gruber-darker-theme
  :vc ( :url "http://github.com/rexim/gruber-darker-theme"
        :branch "master"
        :rev :newest)
  :defer t
  :commands (load-gruber-theme)
  :bind ("<f6>" . load-gruber-theme)
  :init
  (defun load-gruber-theme ()
    (interactive)
    (disable-themes)
    (load-theme 'gruber-darker t)))

(use-package gruber-darker-theme
  :unless window-system
  :init (load-theme 'gruber-darker t))

(use-package corfu
  :ensure t
  :defer t)

(use-package corfu-terminal
  :vc ( :url "https://codeberg.org/akib/emacs-corfu-terminal.git"
        :branch "master")
  :unless window-system
  :config (corfu-terminal-mode +1))

(use-package multiple-cursors
  :ensure t
  :bind (("C-S-c C-S-c" . mc/edit-lines)
         ("C->" . mc/mark-next-like-this)
         ("C-<" . mc/mark-previous-like-this)
         ("C-c C-<" . mc/mark-all-like-this)
         ("C-S-<mouse-1>" . mc/add-cursor-on-click)))

(use-package display-line-numbers
  :defer t
  :hook (prog-mode . display-line-numbers-mode)
  :custom
  (display-line-numbers-type 'relative))

(use-package simple
  :no-require t
  :bind (("C-M-y" . clipboard-yank)
         ("C-x M-c" . put-file-name-to-clipboard)
         ("C-," . duplicate-line)
         ("C-." . copy-line))
  :custom
  (blink-matching-delay 0)
  (blink-matching-paren t)
  :config
  (defun put-file-name-to-clipboard ()
    (interactive)
    (let ((file-name (buffer-file-name)))
      (when file-name
        (kill-new file-name))))
  (defun duplicate-line ()
    (interactive)
    (move-beginning-of-line 1)
    (kill-line)
    (yank)
    (newline)
    (yank))
  (defun copy-line()
    (interactive)
    (let ((prev-pos (point)))
      (back-to-indentation)
      (let* ((start (point))
             (_ (end-of-line))
             (end (point))
             (selected-text (buffer-substring start end)))
        (kill-new selected-text))
      (goto-char prev-pos))))

(use-package diff-mode
  :defer t
  :config
  (keymap-unset diff-mode-shared-map "o"))

(use-package vertico
  :ensure t
  :bind ( :map vertico-map
          ("M-RET" . vertico-exit-input))
  :init
  (vertico-mode))

(use-package common-lisp-modes
  :delight common-lisp-modes-mode
  :preface
  (define-minor-mode common-lisp-modes-mode
    "Mode for enabling all modes that are common for lisps.
For reference, this is not a common-lisp modes mode, but a common
lisp-modes mode.
\\<common-lisp-modes-mode-map>"
    :lighter " clmm"
    :keymap (make-sparse-keymap))
  (provide 'common-lisp-modes))

(use-package puni
  :ensure t
  :hook (((common-lisp-modes-mode prog-mode sgml-mode) . puni-mode)
         (puni-mode-hook . electric-pair-local-mode))
  :bind (:map puni-mode-map
              ("M-r" . puni-raise)
              ("M-(" . puni-wrap-round)
              ("M-{" . puni-wrap-curly)
              ("M-s" . puni-splice)
              ("C-)" . puni-slurp-forward)
              ("C-(" . puni-slurp-backward)))

(use-package puni
  :when window-system
  :bind ( :map puni-mode-map
          ;; doesn't work in terminal
          ("M-[" . puni-wrap-square)))

(use-package vc-hooks
  :no-require t
  :custom (vc-follow-symlinks t))

(use-package dired
  :custom
  (dired-kill-when-opening-new-dired-buffer t)
  (dired-listing-switches "-lAXhv --group-directories-first")
  :config
  (defun kill-dired-buffer ()
    (interactive)
    (let ((dired-buffer (current-buffer)))
      (kill-buffer dired-buffer)))
  (defun dired-home-directory ()
    (interactive)
    (kill-dired-buffer)
    (dired (expand-file-name "~/")))
  :bind (:map dired-mode-map
              ("<backspace>" . dired-up-directory)
              ("~" . dired-home-directory)
              ("q" . kill-dired-buffer)))

(use-package yasnippet
  :ensure t
  :defer t
  :config
  (yas-reload-all)
  :custom
  (yas-snippet-dirs '("~/.config/emacs/snippets")))

(use-package eglot
  :defer t
  :no-require t
  :custom
  (eglot-autoshutdown t)
  (eglot-extend-to-xref t))

(use-package flycheck
  :ensure t
  :defer t)

(use-package lsp-mode
  :defer t
  :ensure t
  :hook (lsp-mode . yas-minor-mode)
  :commands (lsp lsp-deferred)
  :custom
  (lsp-keymap-prefix "C-c l")
  (lsp-ui-doc-show-with-mouse nil)
  (lsp-lens-enable nil)
  (lsp-completion-provider :none)
  (lsp-signature-render-documentation nil))

(use-package clojure-mode
  :ensure t
  :hook ((clojure-mode clojurec-mode clojurescript-mode) . common-lisp-modes-mode)
  :custom
  (clojure-indent-style 'always-indent))

(use-package cider
  :ensure t
  :defer t
  :hook (((cider-repl-mode cider-mode) . eldoc-mode)
         (cider-repl-mode . common-lisp-modes-mode))
  :bind ( :map cider-mode-map
          ("M-RET" . cider-inspect-last-result)
          ("C-c M-;" . cider-pprint-eval-last-sexp-to-comment)
          :map cider-insert-commands-map
          ("C-j" . send-to-repl)
          ("j" . send-to-repl))
  :config
  (defun send-to-repl ()
    (interactive)
    (let ((sexp (copy-backward-sexp)))
      (cider-switch-to-repl-buffer)
      (insert sexp)
      (cider-repl-return)
      (cider-switch-to-last-clojure-buffer)))
  :custom
  (cider-save-file-on-load nil)
  (cider-print-fn 'fipp)
  (cider-repl-display-help-banner nil)
  (nrepl-hide-special-buffers t)
  (cider-allow-jack-in-without-project t)
  (cider-font-lock-dynamically '(macro deprecated var))
  (cljr-suppress-no-project-warning t))

(use-package clj-refactor
  :ensure t
  :defer t
  :hook ((clj-refactor-mode . yas-minor-mode)
         (cider-mode . clj-refactor-mode))
  :config
  (cljr-add-keybindings-with-prefix "C-c C-n")
  :custom
  (cljr-magic-requires nil)
  (cljr-add-ns-to-blank-clj-files nil))

(use-package elisp-mode
  :defer t
  :hook ((emacs-lisp-mode . common-lisp-modes-mode))
  :bind ( :map emacs-lisp-mode-map
          ("C-c C-m" . pp-macroexpand-last-sexp)
          ("C-c C-c" . eval-region)
          :map lisp-interaction-mode-map
          ("C-c C-m" . pp-macroexpand-last-sexp)
          ("C-c C-c" . eval-region)))

(use-package asm-mode
  :defer t
  :mode (("\\.asm\\'" . asm-mode)
         ("\\.inc\\'" . asm-mode))
  :hook (asm-mode . unset-comment-char)
  :config
  (defun unset-comment-char ()
    (local-unset-key (vector asm-comment-char)))
  :custom
  (tab-width 2))

(use-package pascal-mode
  :defer t
  ;:mode (("\\.pp\\'" . pascal-mode))
  :custom
  (pascal-indent-level 2)
  (pascal-auto-lineup nil))

(use-package opascal
  :defer t
  :mode (("\\.pas\\'" . opascal-mode))
  :custom
  (opascal-indent-level 2))

(use-package sgml-mode
  :defer t
  :hook (sgml-mode . yas-minor-mode)
  :bind
  ( :map html-mode-map
    ("C-c C-b" . sgml-skip-tag-backward)
    ("C-c C-c d" . html-div))
  :bind-keymap
  ("C-c o" . facemenu-keymap)
  :config
  (keymap-unset html-mode-map "M-o")
  (sgml-electric-tag-pair-mode 1))

(use-package magit
  :defer t
  :ensure t)

(use-package esh-mode
  :defer t
  :init
  (defun eshell-add-aliases ()
    (dolist (al '(("ll" "ls -la $*")
                  ("fasm" "fasm.x64 $*")))
      (add-to-list 'eshell-command-aliases-list al)))
  :hook ((eshell-post-command . eshell-add-aliases)))

(use-package sh-script
  :defer t
  :hook (sh-mode . yas-minor-mode))

(use-package markdown-mode
  :defer t
  :ensure t
  :mode ("README\\.md\\'" . gfm-mode)
  :init (setq markdown-command "pandoc")
  :config
  (keymap-unset markdown-mode-map "M-p")
  (keymap-unset markdown-mode-map "M-n")
  :bind (:map markdown-mode-map
              ("C-c C-e" . markdown-do)))

(use-package smtpmail
  :defer t
  :custom
  (smtpmail-default-smtp-server "smtp.gmail.com")
  (smtpmail-smtp-server "smtp.gmail.com")
  (smtpmail-smtp-service 587))

(use-package message
  :defer t
  :custom
  (message-send-mail-function 'smtpmail-send-it))

(use-package sendmail
  :defer t
  :custom
  (send-mail-function 'smtpmail-send-it))

(use-package sly
  :defer t
  :ensure t
  :hook ((sly-mode . common-lisp-modes-mode))
  :custom
  (inferior-lisp-program "/home/mx/.local/bin/sbcl")
  :config
  (defun eval-last-sexp-in-mrepl ()
    (interactive)
    (let* ((sexp-end (point))
           (_ (backward-sexp))
           (sexp-begin (point))
           (sexp (buffer-substring-no-properties sexp-begin sexp-end)))
      (goto-char sexp-end)
      (other-window 1)
      (sly-mrepl #'switch-to-buffer)
      (goto-char (point-max))
      (insert sexp)
      (sly-mrepl-return)
      (other-window -1)))
  :bind ( :map sly-mode-map
          ("C-c C-j" . eval-last-sexp-in-mrepl)
          ("C-c M-;" . sly-eval-print-last-expression)
          ("C-c C-q" . sly-quit-lisp)
          :map sly-editing-mode-map
          ("M-p" . nil)
          ("M-n" . nil)))

(use-package geiser-guile
  :defer t
  :ensure t
  :hook ((geiser-mode . common-lisp-modes-mode))
  :custom
  (geiser-guile-binary (executable-find "guile")))

(use-package geiser-racket
  :defer t
  :ensure t
  :hook ((geiser-mode . common-lisp-modes-mode))
  :custom
  (geiser-racket-binary (executable-find "racket")))

(use-package gdb-mi
  :custom
  (gdb-many-windows t))

(use-package js-comint
  :after js
  :ensure t)

(use-package js
  :defer t
  :hook ((js-mode . corfu-mode))
  :mode (("\\.js\\'"  . js-mode)
         ("\\.mjs\\'" . js-mode))
  :bind ( :map js-mode-map
          ("C-c C-e" . #'js-comint-send-last-sexp)
          ("C-c C-r" . #'js-comint-send-region)
          ("C-c C-b" . #'js-comint-send-buffer)
          ("C-c C-z" . #'js-comint-start-or-switch-to-repl)
          ("M-." . #'lsp-find-definition))
  :custom
  (js-indent-level 2)
  (js-switch-indent-offset 2))

(use-package typescript-ts-mode
  :defer t
  :mode (("\\.ts\\'" . typescript-ts-mode)
         ("\\.tsx\\'" . tsx-ts-mode))
  :init (defalias 'ts-mode 'typescript-ts-mode)
  :hook ((typescript-ts-mode . lsp-deferred)
         (typescript-ts-mode . corfu-mode)
         (tsx-ts-mode . corfu-mode))
  :custom
  (typescript-indent-level 2))

(use-package zig-mode
  :defer t
  :vc ( :url "https://github.com/ziglang/zig-mode"
        :branch "master"
        :rev :newest)
  :mode ("\\.zig\\'" . zig-mode))

(use-package go-mode
  :defer t
  :ensure t
  :mode ("\\.go\\'" . go-mode)
  :bind ( :map go-mode-map
          ("M-." . #'godef-jump)
          ("C-c C-d d" . #'godef-describe)
          ("C-c C-d c" . #'godoc-at-point)
          ("C-c C-d C-c" . #'godoc-at-point))
  :config
  (keymap-unset go-mode-map "C-c C-d"))

(use-package lua-mode
  :ensure t
  :defer t
  :mode (("\\.lua\\'" . lua-mode)
         ("\\.rockspec\\'" . lua-mode))
  :bind ( :map lua-prefix-mode-map
          ("C-e" . lua-send-current-line)
          ("C-r" . lua-send-region))
  :custom
  (lua-indent-level 2)
  (lua-default-application "luajit"))

(use-package ggtags
  ;; Need to build and install ctags, GNU Global as described in ggtags doc
  ;; Navigate through system includes:
  ;; - mkdir  ~/.gtags && cd ~/.gtags
  ;; - ln -s /usr/include usr-include
  ;; - gtags -c
  ;; Then update project's GTAGS
  :ensure t
  :defer t
  :init
  (setenv "GTAGSLIBPATH" (expand-file-name "~/.gtags"))
  :config
  (keymap-unset ggtags-navigation-map "M-o")
  (keymap-unset ggtags-navigation-map "M->")
  (keymap-unset ggtags-navigation-map "M-<"))

(use-package cc-mode
  :defer t
  :hook (c-mode-common . ggtags-mode))

(use-package css-mode
  :defer t
  :custom
  (css-indent-offset 2))

(use-package sql-indent
  :ensure t
  :defer t)

(use-package sql
  :defer t
  :requires sql-indent
  :hook (sql-mode . yas-minor-mode))

(use-package sml-mode
  :defer t
  :ensure t
  :custom
  (sml-indent-level 2))

(use-package merlin
  :defer t
  :load-path (lambda () (expand-file-name "~/data/sources/merlin/emacs"))
  :hook (merlin-mode . corfu-mode)
  :autoload merlin-mode
  :custom
  (merlin-report-warnings t))

(use-package caml
  :defer t
  :ensure t
  :hook (caml-mode . merlin-mode))

(use-package tuareg
  :defer t
  :ensure t
  :hook (tuareg-mode . merlin-mode))

(use-package rustic
  :defer t
  :vc ( :url "https://github.com/brotzeit/rustic" :branch "master")
  :mode ("\\.rs\\'" . rustic-mode)
  :init (setq rust-mode-treesitter-derive t)
  :hook ((rustic-mode . lsp-deferred)
         (rustic-mode . corfu-mode)
         (rustic-mode . puni-mode))
  :custom
  (rustic-analyzer-command '("rustup" "run" "stable" "rust-analyzer")))
