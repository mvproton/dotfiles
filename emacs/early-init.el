;;; early-init.el --- Early initialization -*- lexical-binding: t; no-byte-compile: t -*-

(defvar gc-cons-threshold-old gc-cons-threshold)

(setq gc-cons-threshold most-positive-fixnum ; old value is 800000
      read-process-output-max (* 1024 1024 4) ; 4mb
      load-prefer-newer t
      package-enable-at-startup nil
      message-log-max (* 1024 16) ; 16 kb
      inhibit-compacting-font-caches t)

(modify-all-frames-parameters
 '((menu-bar-lines . 0)
   (vertical-scroll-bars . nil)
   (tool-bar-lines . 0)
   (width . 128)
   (height . 68)))

;; Fix problem with gpg on Win10
(when (memq system-type '(windows-nt))
  (setq package-gnupghome-dir "~/.config/emacs/elpa/gnupg"))

(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)
