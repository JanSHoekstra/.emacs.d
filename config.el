;;; config.el --- Jan Hoekstra's Emacs Setup  -*- lexical-binding: t; -*-

;;; Commentary:
;; This file includes my configuration, divided per package.

;;; Code:

(use-package emacs
  :ensure nil
  :config

  ;; Disable UI nonsense.
  (tool-bar-mode 0)
  (menu-bar-mode 0)
  (blink-cursor-mode 0)

  (setq
   ;; No need for the default startup screen.
   inhibit-startup-screen t
   ;; I'd like my scratch clean, please.
   initial-scratch-message nil
   ;; No annoying, distracting noises.
   ring-bell-function 'ignore
   )

  (setq-default indent-tabs-mode 0
				tab-width 4
				truncate-lines 1
				truncate-partial-width-windows 1))

(use-package challenger-deep-theme :config (load-theme 'challenger-deep 't))

;; Themes like to reset font configuration,
;; So load fonts afterwards.
(use-package emacs
  :config
  (set-face-attribute 'default nil :family "iA Writer Mono V")
  (set-face-attribute 'mode-line nil :family "iA Writer Quattro V" ))

(use-package scroll-bar
  :ensure nil
  :config (scroll-bar-mode 0))

(use-package simple
  :ensure nil
  :config (column-number-mode +1))

(use-package files
  :ensure nil
  :config
  (setq confirm-kill-processes nil
	create-lockfiles nil
	make-backup-files nil))

(use-package elec-pair
  :ensure nil
  :hook (prog-mode . electric-pair-mode))

(use-package whitespace
  :ensure nil
  :hook (before-save . whitespace-cleanup))

(use-package dired
  :ensure nil
  :config
;; Delete intermediate buffers when navigating through dired.
  (setq delete-by-moving-to-trash t)
  (put 'dired-find-alternate-file 'disabled nil)
  (define-key dired-mode-map (kbd "RET") 'dired-find-alternate-file))

(use-package org
  :ensure nil
  :config
  (add-hook 'org-mode-hook (lambda ()
							(defvar buffer-face-mode-face '(:family "iA Writer Quattro V"))
							(buffer-face-mode))))

(use-package ido
  :ensure nil
  :config
  (ido-mode +1)
  (setq ido-everywhere t
		ido-enable-flex-matching t))

(use-package ido-vertical-mode
  :ensure nil
  :config
  (ido-vertical-mode +1)
  (setq ido-vertical-define-keys 'C-n-C-p-up-and-down))

(use-package ido-completing-read+ :config (ido-ubiquitous-mode +1))

(use-package flx-ido :config (flx-ido-mode +1))

(use-package company
  :hook (prog-mode . company-mode)
  :config
  (setq company-minimum-prefix-length 1
		company-idle-delay 0.1
		company-selection-wrap-around t
		company-tooltip-align-annotations t
		company-frontends '(company-pseudo-tooltip-frontend ; show tooltip even for single candidate
							company-echo-metadata-frontend))
  (define-key company-active-map (kbd "C-n") 'company-select-next)
  (define-key company-active-map (kbd "C-p") 'company-select-previous))

(use-package flycheck :config (global-flycheck-mode +1))

(use-package projectile
  :hook (prog-mode . projectile-mode)
  :config
  (define-key projectile-mode-map (kbd "<f5>") 'projectile-compile-project)
  (define-key projectile-mode-map (kbd "<f6>") 'projectile-run-project)
  )

(use-package centered-cursor-mode
  :hook (prog-mode . centered-cursor-mode))

(provide 'config)
;;; config.el ends here
