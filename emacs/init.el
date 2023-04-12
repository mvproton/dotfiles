;;; init.el --- Main configuration file -*- lexical-binding: t; no-byte-compile: t -*-

(use-package straight)
(use-package delight :straight t)

(use-package gcmh
  :straight t
  :hook (after-init . gcmh-mode)
  :delight gcmh-mode)

(use-package emacs
  :init
  (setq-default indent-tabs-mode nil
                tab-width 4
                truncate-lines t
                inhibit-startup-screen t
                ring-bell-function 'ignore
                eval-expression-print-level nil
                eval-expression-print-length nil
                hscroll-step 1
                hscroll-margin 0
                warning-minimum-level :error
                initial-scratch-message ";;; Scratch buffer\n\n"
                default-input-method "russian-computer")
  (setq ring-bell-function 'ignore
        enable-recursive-minibuffers t)
  :bind (("M-g a". "α")
         ("M-g b". "β")
         ("M-g g". "γ")
         ("M-g d". "δ")
         ("M-g e". "ε")
         ("M-g z". "ζ")
         ("M-g h". "η")
         ("M-g q". "θ")
         ("M-g i". "ι")
         ("M-g k". "κ")
         ("M-g l". "λ")
         ("M-g m". "μ")
         ("M-g n". "ν")
         ("M-g x". "ξ")
         ("M-g o". "ο")
         ("M-g p". "π")
         ("M-g r". "ρ")
         ("M-g s". "σ")
         ("M-g t". "τ")
         ("M-g u". "υ")
         ("M-g f". "ϕ")
         ("M-g j". "φ")
         ("M-g c". "χ")
         ("M-g y". "ψ")
         ("M-g w". "ω")
         ("M-g A". "Α")
         ("M-g B". "Β")
         ("M-g G". "Γ")
         ("M-g D". "Δ")
         ("M-g E". "Ε")
         ("M-g Z". "Ζ")
         ("M-g H". "Η")
         ("M-g Q". "Θ")
         ("M-g I". "Ι")
         ("M-g K". "Κ")
         ("M-g L". "Λ")
         ("M-g M". "Μ")
         ("M-g N". "Ν")
         ("M-g X". "Ξ")
         ("M-g O". "Ο")
         ("M-g P". "Π")
         ("M-g R". "Ρ")
         ("M-g S". "Σ")
         ("M-g T". "Τ")
         ("M-g U". "Υ")
         ("M-g F". "Φ")
         ("M-g J". "Φ")
         ("M-g C". "Χ")
         ("M-g Y". "Ψ")
         ("M-g W". "Ω")))

(use-package startup
  :no-require t
  :custom
  (user-mail-address "mvproton@gmail.com")
  (user-full-name "Maks"))

(use-package simple
  :no-require t
  :bind (("C-M-y" . clipboard-yank)
         ("C-x M-c" . put-file-name-to-clipboard))
  :config
  (defun put-file-name-to-clipboard ()
    (interactive)
    (let ((file-name (buffer-file-name)))
      (when file-name
        (kill-new file-name)))))

(use-package cus-edit
  :custom
  (custom-file (expand-file-name "custom.el" user-emacs-directory))
  :init
  (load custom-file :noerror))

(use-package subr
  :no-require t
  :init
  (fset 'yes-or-no-p 'y-or-n-p))

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

(use-package vc-hooks
  :no-require t
  :custom (vc-follow-symlinks t))

(use-package modus-themes
  :straight t
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
  (load-theme 'modus-operandi t)
  :config
  (set-face-attribute 'default nil :font "Fira Code")
  (set-face-attribute 'show-paren-match nil :weight 'extra-bold)
  :bind ("<f5>" . modus-themes-toggle))

(use-package files
  :preface
  (defvar backup-dir
    (expand-file-name ".cache/backups" user-emacs-directory)
    "Directory to store backups.")
  (defvar auto-save-dir
    (expand-file-name ".cache/auto-save/" user-emacs-directory)
    "Directory to store auto-save files.")
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

(use-package functions
  :defer t
  :preface
  (defun edit-init-file ()
    (interactive)
    (find-file (expand-file-name "init.el" user-emacs-directory)))
  (defun edit-early-init-file ()
    (interactive)
    (find-file (expand-file-name "early-init.el" user-emacs-directory)))
  (defun memoize (fn)
    (let ((memo (make-hash-table :test 'equal)))
      (lambda (&rest args)
        (let ((value (gethash args memo)))
          (or value (puthash args (apply fn args) memo))))))
  (provide 'functions))

(use-package vertico
  :straight t
  :load-path "straight/repos/vertico/extensions/"
  :bind (:map vertico-map
              ("M-RET" . vertico-exit-input))
  :init
  (vertico-mode))

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

(use-package clojure-mode
  :straight t
  :hook ((clojure-mode clojurec-mode clojurescript-mode) . common-lisp-modes-mode))

(use-package cider
  :straight t
  :defer t
  :hook (((cider-repl-mode cider-mode) . eldoc-mode)
         (cider-repl-mode . common-lisp-modes-mode))
  :bind (:map cider-mode-map
              ("M-RET" . cider-inspect-last-result)
              ("C-c M-;" . cider-pprint-eval-last-sexp-to-comment))
  :custom
  (cider-save-file-on-load nil)
  (cider-print-fn 'fipp)
  (cider-repl-display-help-banner nil)
  (nrepl-hide-special-buffers t)
  (cider-allow-jack-in-without-project t)
  (cider-font-lock-dynamically '(macro deprecated var))
  (cljr-suppress-no-project-warning t))

(use-package clj-refactor
  :straight t
  :defer t
  :hook ((clj-refactor-mode . yas-minor-mode)
         (cider-mode . clj-refactor-mode))
  :config
  (cljr-add-keybindings-with-prefix "C-c C-n")
  :custom
  (cljr-magic-requires nil)
  (cljr-add-ns-to-blank-clj-files nil))

(use-package puni
  :straight t
  :hook ((common-lisp-modes-mode . puni-mode)
         (puni-mode . electric-pair-mode))
  :bind (:map puni-mode-map
              ("M-r" . puni-raise)
              ("M-(" . puni-wrap-round)
              ("M-[" . puni-wrap-square)
              ("M-{" . puni-wrap-curly)
              ("M-s" . puni-splice)
              ("C-)" . puni-slurp-forward)
              ("C-(" . puni-slurp-backward)))

(use-package elisp-mode
  :defer t
  :hook ((emacs-lisp-mode . common-lisp-modes-mode))
  :bind ( :map emacs-lisp-mode-map
          ("C-c C-m" . pp-macroexpand-last-sexp)
          :map lisp-interaction-mode-map
          ("C-c C-m" . pp-macroexpand-last-sexp)))

(use-package asm-mode
  :defer t
  :custom
  (tab-width 2))

(use-package pascal-mode
  :defer t
  :mode (("\\.pp\\'" . pascal-mode))
  :custom
  (pascal-indent-level 2)
  (pascal-auto-lineup nil))

(use-package opascal
  :defer t
  :custom
  (opascal-indent-level 2))

(use-package sgml-mode
  :defer t
  :bind-keymap
  ("C-c o" . facemenu-keymap)
  :config
  (keymap-unset html-mode-map "M-o")
  (sgml-electric-tag-pair-mode 1))

(use-package magit
  :defer t
  :straight t)

(use-package elpy
  :defer t
  :straight t
  :init (advice-add 'python-mode :before 'elpy-enable))

(use-package esh-mode
  :defer t
  :config
  ;; (defalias 'll "ls -la $*")
  ;; (defalias 'aa "echo 'TEST'")
  ;; Сделать так что бы при установке записывались алиасы.
  )

(use-package markdown-mode
  :defer t
  :straight t
  :mode ("README\\.md\\'" . gfm-mode)
  :init (setq markdown-command "multimarkdown")
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
  :straight t
  :hook ((sly-mode . common-lisp-modes-mode))
  :custom
  (inferior-lisp-program "/home/mx/.local/bin/sbcl")
  :preface
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
          ("C-c C-q" . sly-quit-lisp)))

(use-package geiser-guile
  :defer t
  :straight t
  :hook ((geiser-mode . common-lisp-modes-mode))
  :custom
  (geiser-guile-binary (executable-find "guile")))

(use-package geiser-racket
  :defer t
  :straight t
  :hook ((geiser-mode . common-lisp-modes-mode))
  :custom
  (geiser-racket-binary (executable-find "racket")))
