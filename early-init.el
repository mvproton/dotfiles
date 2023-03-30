;;; early-init.el --- Early initialization -*- lexical-binding: t; no-byte-compile: t -*-

(defvar gc-cons-threshold-old gc-cons-threshold)

(setq gc-cons-threshold most-positive-fixnum ; old value is 800000
      read-process-output-max (* 1024 1024 4) ; 4mb
      load-prefer-newer t
      package-enable-at-startup nil
      message-log-max (* 1024 16) ; 16 kb
      inhibit-compacting-font-caches t)

(setq-default default-frame-alist '((width . 170)
				    (height . 56)
				    (tool-bar-lines . 0)
				    (bottom-divider-width . 0)
				    (right-divider-width . 1))
	      frame-inhibit-implied-resize t
	      fringe-indicator-alist (assq-delete-all 'truncation fringe-indicator-alist))

(scroll-bar-mode -1)
(menu-bar-mode -1)
(tool-bar-mode -1)

(add-hook 'after-init-hook (lambda ()
                             (setq gc-cons-threshold gc-cons-threshold-old)))


;; Straight

(defvar straight-repository-branch)
(setq straight-repository-branch "develop")

(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 6))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

(provide 'early-init)
