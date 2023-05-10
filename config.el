;;; config.el --- Zoë Hoekstra's Emacs Setup  -*- lexical-binding: t; -*-


;;; Commentary:
;; This file includes my configuration, divided per package.

;;; Code:

;;(load "~/.emacs.d/splash")

(use-package emacs
  :ensure nil
  :config

  ;; Disable UI nonsense.
  (tool-bar-mode 0)
  (menu-bar-mode 0)
  (blink-cursor-mode 0)
  (tooltip-mode 0)

  (setq
   ;; No need for the default startup screen.
   inhibit-startup-screen t
   ;; I'd like my scratch clean, please.
   initial-scratch-message nil
   ;; No annoying, distracting noises.
   ring-bell-function 'ignore
   ;; Emacs 28 sets this to 1, a little too slow for classic scrollwheels
   mouse-wheel-scroll-amount '(3)
   ;; Allow Emacs to scale pixelwise, otherwise it looks a little odd in WMs
   frame-resize-pixelwise t
   ;; On long lines, keep the cursor in the middle of the screen (horizontally)
   hscroll-margin 9999
   )

  (setq-default indent-tabs-mode 0
				tab-width 4
				truncate-lines 1
				truncate-partial-width-windows 1))

(if (eq system-type 'darwin)
  (use-package modus-themes :config (load-theme 'modus-operandi 't))
  (use-package challenger-deep-theme :config (load-theme 'challenger-deep 't)))

;; Themes like to reset font configuration,
;; So load fonts afterwards.
(use-package emacs
  :config
  (if (not (eq system-type 'windows-nt))
	  (progn
		(set-face-attribute 'default nil :family "iA Writer Mono S" :height (if (> (x-display-pixel-height) 1080) 130 110))
		(set-face-attribute 'mode-line nil :family "iA Writer Quattro S"))))

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
  :config (defun clean-whitespace-in-prog-buffers ()
			  (when (derived-mode-p major-mode)
				(whitespace-cleanup)))
  :hook (before-save . clean-whitespace-in-prog-buffers))

(use-package dired-sidebar
  :bind (("C-x C-n" . dired-sidebar-toggle-sidebar))
  :ensure t
  :commands (dired-sidebar-toggle-sidebar)
  :init
  (add-hook 'dired-sidebar-mode-hook
			(lambda ()
			  (unless (file-remote-p default-directory)
				(auto-revert-mode))))
  :config
  (push 'toggle-window-split dired-sidebar-toggle-hidden-commands)
  (push 'rotate-windows dired-sidebar-toggle-hidden-commands)

  (setq dired-sidebar-subtree-line-prefix "__")
  ;; (setq dired-sidebar-theme 'vscode)
  (setq dired-sidebar-use-term-integration t)
  (setq dired-sidebar-use-custom-font t))

(use-package org
  :ensure nil
  :config
  ;; Always be able to open the agenda through it's shortcut
  (global-set-key "\C-ca" 'org-agenda)
  (global-set-key "\C-cl" 'org-store-link)
  (setq org-agenda-files '("~/Sync/org"))
  ;; Add all the events Emacs knows about
  (defvar org-agenda-include-diary t)


  ;; Use the Quattro font only for org buffers
  (add-hook 'org-mode-hook (lambda ()
							 (defvar buffer-face-mode-face '(:family "iA Writer Quattro S"))
							 (buffer-face-mode))))

(use-package org-protocol
  :ensure nil)

(use-package ido
  :ensure nil
  :config
  (ido-mode +1)
  (setq ido-everywhere t
		ido-enable-flex-matching t))

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
  (define-key company-active-map (kbd "C-p") 'company-select-previous)
  (define-key company-active-map (kbd "C-d") 'company-show-doc-buffer)
  (define-key company-active-map (kbd "M-.") 'company-show-location))

(use-package flycheck :config (global-flycheck-mode +1))

(use-package projectile
  :hook (prog-mode . projectile-mode)
  :config
  (define-key projectile-mode-map (kbd "<f5>") 'projectile-compile-project)
  (define-key projectile-mode-map (kbd "<f6>") 'projectile-run-project))

(use-package centered-cursor-mode
  :hook (prog-mode . centered-cursor-mode)
  :config (setq ccm-recenter-at-end-of-file t
				ccm-recenter-end-of-file t))

(use-package slime
  :init
  (add-hook 'lisp-mode-hook (lambda () (slime-mode t)))
  (slime-setup '(slime-fancy slime-company))
  :config
  (setq inferior-lisp-program "sbcl"))

(use-package slime-company
  :after (slime company)
  :config
  (setq slime-company-completion 'fuzzy))

(use-package markdown-mode
  :init
  (add-hook 'markdown-mode-hook (lambda ()
								  (defvar buffer-face-mode-face '(:family "iA Writer Quattro V"))
								  (buffer-face-mode)
								  (visual-line-mode)))
  :config
  (custom-set-faces
   '(markdown-header-face ((t (:inherit font-lock-function-name-face :weight bold :family "variable-pitch"))))
   '(markdown-header-face-1 ((t (:inherit markdown-header-face :height 1.8))))
   '(markdown-header-face-2 ((t (:inherit markdown-header-face :height 1.4))))
   '(markdown-header-face-3 ((t (:inherit markdown-header-face :height 1.2)))))

  (setq markdown-hide-markup t)
  (setq markdown-header-scaling t)
  (setq markdown-inline-image-overlays t))

(use-package typescript-mode
  :mode "\\.tsx?$")

(use-package tide
  :ensure t
  :after (typescript-mode company flycheck)
  :hook ((typescript-mode . tide-setup)
		 (typescript-mode . tide-hl-identifier-mode)
		 (before-save . tide-format-before-save)))

(use-package exec-path-from-shell
  :ensure t
  :config
  (when (memq window-system '(mac ns x))
	(exec-path-from-shell-initialize)))

(use-package evil
  :ensure t
  :init
  ;;(evil-mode)
  )

(use-package clojure-mode)
(use-package cider
  :config
  (add-hook 'clojure-mode-hook #'cider-mode)

  (defun clerk-show ()
	(interactive)
	(when-let
		((filename
		  (buffer-file-name)))
	  (save-buffer)
	  (cider-interactive-eval
	   (concat "(nextjournal.clerk/show! \"" filename "\")"))))

  (define-key clojure-mode-map (kbd "<M-return>") 'clerk-show)
  )

(use-package paredit
  :config
  (add-hook 'clojure-mode-hook 'paredit-mode)
  )
(use-package origami
  :init
  (global-origami-mode))
(use-package inf-clojure
  :init
  (defun cljs-node-repl ()
	(interactive)
	(inf-clojure "clj -M -m cljs.main -co build.edn -re node -r"))
  )
(use-package rainbow-delimiters
  :config
  (global-set-key "\C-cr" 'rainbow-delimiters-mode))

(use-package aggressive-indent
  :config
  (global-set-key "\C-ci" 'aggressive-indent-mode))

(use-package haskell-mode)

(use-package eglot
  :ensure nil
  :config
  (global-set-key "\C-cl" 'eglot)
  (add-hook 'haskell-mode-hook 'eglot-ensure)
  (add-hook 'clojure-mode-hook 'eglot-ensure)
  (setq-default eglot-workspace-configuration
				'((haskell
				   (plugin
					(stan
					 (globalOn . :json-false))))))  ;; disable stan
  :custom
  (eglot-autoshutdown t)  ;; shutdown language server after closing last file
  (eglot-confirm-server-initiated-edits nil)  ;; allow edits without confirmation
)

(use-package org-roam
  :ensure t
  :custom
  (org-roam-directory (file-truename "~/notes"))
  :bind (("C-c n l" . org-roam-buffer-toggle)
		 ("C-c n f" . org-roam-node-find)
		 ("C-c n g" . org-roam-graph)
		 ("C-c n i" . org-roam-node-insert)
		 ("C-c n c" . org-roam-capture)
		 ;; Dailies
		 ("C-c n j" . org-roam-dailies-capture-today))
  :config
  ;; If you're using a vertical completion framework, you might want a more informative completion interface
  (setq org-roam-node-display-template (concat "${title:*} " (propertize "${tags:10}" 'face 'org-tag)))
  (org-roam-db-autosync-mode)
  ;; If using org-roam-protocol
  (require 'org-roam-protocol))

(use-package plantuml-mode
  :custom
  (add-to-list 'auto-mode-alist '("\\.plantuml\\'" . plantuml-mode))
  (add-to-list 'org-src-lang-modes '("plantuml" . plantuml))
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((plantuml . t)))
  :config
  (setq plantuml-default-exec-mode 'exec)
  )

(provide 'config)
;;; config.el ends here
