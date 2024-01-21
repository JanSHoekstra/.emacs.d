;;; config.el --- Zoë Hoekstra's Emacs Setup  -*- lexical-binding: t; -*-

;;; Commentary:
;; This file includes my configuration, divided per package.

;;; Code:

;;;;;;;; Emacs

(use-package modus-themes :config (load-theme 'modus-vivendi-tinted 't))

(use-package emacs
  :diminish auto-revert-mode
  :ensure nil
  :config

  (if (not (eq system-type 'windows-nt))
	  (progn
		(set-face-attribute 'default
							nil
							:family "iA Writer Mono S"
							:height 120)
		(set-face-attribute 'mode-line
							nil
							:family "iA Writer Quattro S")))

  ;; Disable UI nonsense.
  (tool-bar-mode 0)
  (menu-bar-mode 0)
  (blink-cursor-mode 0)
  (tooltip-mode 0)
  (scroll-bar-mode 0)

  (set-face-attribute 'mode-line nil :box nil)
  (set-face-attribute 'mode-line-inactive nil :box nil)

  ;; Enable mouse in terminal
  (xterm-mouse-mode 1)

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
   ;; Symlinks to version controlled files should just be followed, don't ask.
   vc-follow-symlinks t)

  (setq-default indent-tabs-mode 0
				tab-width 4
				truncate-lines 1
				truncate-partial-width-windows 1)

  (defun switch-to-minibuffer ()
	"Switch to minibuffer window."
	(interactive)
	(if (active-minibuffer-window)
		(select-window (active-minibuffer-window))
	  (error "Minibuffer is not active")))

  (global-set-key "\C-cm" 'switch-to-minibuffer) ;; Bind to `C-c o'

  ;; Launch frames as maximized
  (add-to-list 'default-frame-alist '(fullscreen . maximized))

  ;; Make use of wayland clipboard when running cli client in Wayland
  ;; https://www.emacswiki.org/emacs/CopyAndPaste
  (when (string= (getenv "XDG_SESSION_TYPE") "wayland")
	(defvar wl-copy-process)
	(setq wl-copy-process nil)

	(defun wl-copy (text)
	  (setq wl-copy-process (make-process :name "wl-copy"
										  :buffer nil
										  :command '("wl-copy" "-f" "-n")
										  :connection-type 'pipe))
	  (process-send-string wl-copy-process text)
	  (process-send-eof wl-copy-process))

	(defun wl-paste ()
	  (if (and wl-copy-process (process-live-p wl-copy-process))
		  nil ; should return nil if we're the current paste owner
		(shell-command-to-string "wl-paste -n | tr -d \r")))

	(setq interprogram-cut-function 'wl-copy)
	(setq interprogram-paste-function 'wl-paste)))

;;;;;;;; Folds

(use-package hs-minor-mode
  :ensure nil
  :hook (prog-mode . hs-minor-mode))

;;;;;;;; General

;; Enable evil vi bindings
(use-package evil
  :ensure t
  :init
  (setq evil-undo-system 'undo-redo)
  (evil-mode)
  (global-set-key (kbd "<escape>") 'keyboard-escape-quit))

;; Useful to remember keybinds
(use-package which-key
  :diminish which-key-mode
  :init
  (which-key-mode)
  (which-key-setup-minibuffer)
  :config
  (setq which-key-idle-delay 0.5))

(use-package general
  :init
  ;; Global keybinds
  (general-define-key
   :states '(normal)
   :keymaps 'override
   :prefix "SPC"
   "1" 'delete-other-windows)
  (general-define-key
   :states 'normal
   :keymaps 'org-mode-map
   :prefix ","
   "t" 'org-todo))

;; Emacs native terminal, with decent behaviour and speed
(use-package eat
  :ensure t)

(use-package magit)

(use-package simple
  :ensure nil
  :config (column-number-mode +1))

(use-package files
  :ensure nil
  :config
  (setq confirm-kill-processes nil
		create-lockfiles nil
		make-backup-files nil))

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

(use-package ido
  :ensure nil
  :config
  (ido-mode +1)
  (setq ido-everywhere t
		ido-enable-flex-matching t))

(use-package ido-completing-read+ :config (ido-ubiquitous-mode +1))

(use-package flx-ido :config (flx-ido-mode +1))

(use-package flx)

(use-package exec-path-from-shell
  :ensure t
  :config
  (when (memq window-system '(mac ns x))
	(exec-path-from-shell-initialize)))

(use-package delight
  :config
  (delight 'eldoc-mode nil "eldoc")
  (delight 'lispyville-mode nil "lispyville")
  (delight 'aggressive-indent-mode nil "aggressive-indent")
  (delight 'evil-vimish-fold-mode nil "evil-vimish-fold")
  (delight 'centered-cursor-mode nil)
  (delight 'hs-minor-mode nil "hide-show")
  (delight 'company-mode nil "company"))

;;;;;;;; General Programming

(use-package elec-pair
  :ensure nil
  :hook (prog-mode . electric-pair-mode))

(use-package whitespace
  :ensure nil
  :config (defun clean-whitespace-in-prog-buffers ()
			(when (derived-mode-p major-mode)
			  (whitespace-cleanup)))
  :hook (before-save . clean-whitespace-in-prog-buffers))

(use-package company
  :hook (prog-mode . company-mode)
  :config
  (setq company-minimum-prefix-length 1
		company-idle-delay 0.1
		company-selection-wrap-around t
		company-tooltip-align-annotations t
		;; show tooltip even for single candidate
		company-frontends '(company-pseudo-tooltip-frontend
							company-echo-metadata-frontend))
  (define-key company-active-map (kbd "C-n") 'company-select-next)
  (define-key company-active-map (kbd "C-p") 'company-select-previous)
  (define-key company-active-map (kbd "C-d") 'company-show-doc-buffer)
  (define-key company-active-map (kbd "M-.") 'company-show-location)
  (define-key company-active-map (kbd "<return>") nil)
  (define-key company-active-map (kbd "RET") nil)
  (define-key company-active-map (kbd "<tab>") 'company-complete-selection)
  (define-key company-active-map (kbd "C-<tab>") 'company-complete-common))

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

(use-package rainbow-delimiters
  :config
  (global-set-key "\C-cr" 'rainbow-delimiters-mode))

(use-package aggressive-indent
  :config
  (global-set-key "\C-ci" 'aggressive-indent-mode)
  :hook ((prog-mode . aggressive-indent-mode)
		 (slime-repl-mode . aggressive-indent-mode)
		 (scheme-mode . aggressive-indent-mode)
		 (emacs-lisp-mode . aggressive-indent-mode)))

(use-package eglot
  :ensure nil
  :config
  (global-set-key "\C-cl" 'eglot)
  (add-hook 'haskell-mode-hook 'eglot-ensure)
  (add-hook 'clojure-mode-hook 'eglot-ensure)
  (add-hook 'scala-mode-hook 'eglot-ensure)
  (setq-default eglot-workspace-configuration
				'((haskell
				   (plugin
					(stan
					 (globalOn . :json-false))))))  ;; disable stan
  :custom
  ;; shutdown language server after closing last file
  (eglot-autoshutdown t)
  ;; allow edits without confirmation
  (eglot-confirm-server-initiated-edits nil))

(use-package eldoc
  :ensure nil
  :config
  ;; When eldoc buffer is open, don't show it in minibuffer anymore.
  (setq eldoc-echo-area-prefer-doc-buffer t))

;; EMacs Refactoring system
(use-package emr
  :ensure t
  :bind ("M-RET" . emr-show-refactor-menu))

;;;;;;;; Writing

(use-package markdown-mode
  :init
  (add-hook 'markdown-mode-hook (lambda ()
								  (defvar buffer-face-mode-face
									'(:family "iA Writer Quattro V"))
								  (buffer-face-mode)
								  (visual-line-mode)))
  :config
  (custom-set-faces
   '(markdown-header-face
	 ((t (:inherit font-lock-function-name-face
				   :weight bold
				   :family "variable-pitch"))))
   '(markdown-header-face-1
	 ((t (:inherit markdown-header-face
				   :height 1.8))))
   '(markdown-header-face-2
	 ((t (:inherit markdown-header-face
				   :height 1.4))))
   '(markdown-header-face-3
	 ((t (:inherit markdown-header-face
				   :height 1.2)))))

  (setq markdown-hide-markup t)
  (setq markdown-header-scaling t)
  (setq markdown-inline-image-overlays t))

(use-package org
  :ensure nil
  :init
  ;; Use the Quattro font only for org buffers
  (add-hook 'org-mode-hook
			(lambda ()
			  (defvar buffer-face-mode-face
				'(:family "iA Writer Quattro V"))
			  (buffer-face-mode)
			  (visual-line-mode)
			  (org-indent-mode)))
  :config
  ;; Always be able to open the agenda through it's shortcut
  (global-set-key "\C-ca" 'org-agenda)
  (global-set-key "\C-cl" 'org-store-link)

  (general-define-key
   :states '(normal)
   :keymaps 'override
   :prefix "SPC"
   "c" 'org-capture
   "a" 'org-agenda)

  (general-define-key
   :states 'normal
   :keymaps 'org-mode-map
   :prefix ","
   "i" 'org-clock-in
   "o" 'org-clock-out
   "s" 'org-insert-structure-template)

  ;; When a TODO is set to a done state, record the time
  (setq org-log-done 'time)

  ;; Org agenda reads these files to fill itself
  (setq org-agenda-files '("~/org"))

  ;; Visually indent headings + content to their level
  ;;(add-hook 'org-mode-hook 'org-indent-mode)

  ;; Wrap lines so that things are easy to read
  ;;(add-hook 'org-mode-hook 'visual-line-mode)

  ;; Add all the events Emacs knows about
  (defvar org-agenda-include-diary nil)

  (setq org-tags-column 0)

  ;; Disable confirmation before code execution
  (setq org-confirm-babel-evaluate nil)

  (defvar org-agenda-custom-commands
	'(("A" "Big agenda"
	   ((agenda "Big agenda"
				((org-agenda-span 16)
				 (org-agenda-start-on-weekday nil)
				 (org-agenda-start-day "-2d")))
		(todo "DOING")
		(todo "TODO")))))

  ;; https://www.gnu.org/software/emacs/manual/html_node/org/Setting-Tags.html
  (setq org-tag-alist '((:startgroup . nil)
						("work" . ?w)
						("personal" . ?p)
						(:endgroup . nil)
						("birthday" . nil)
						("meeting" . ?m)))

  (setq org-todo-keywords
		'((sequence "TODO(t/!)" "WORKING-ON(w/!)" "BLOCK(b/!)"
					"|" "DONE(d@/!)" "NOPE(n@/!)")))

  (setq org-todo-keyword-faces
		'(("TODO" . (:foreground "LightBlue" :weight bold))
		  ("WORKING-ON" . (:foreground "HotPink" :weight bold))
		  ("BLOCK" . (:foreground "thistle" :weight bold))
		  ("DONE" . (:foreground "PaleGreen" :weight bold))
		  ("NOPE" . (:foreground "aquamarine" :weight bold))))

  (defvar org-capture-templates
	'(("l" "Work Log Entry"
	   entry (file+datetree "~/org/work-log.org")
	   "* %?"
	   :tree-type week
	   :empty-lines 0)
	  ("n" "Note"
	   entry (file+headline "~/org/notes.org" "Random Notes")
	   "** %?"
	   :empty-lines 0)
	  ("t" "Todo"
	   entry (file+headline "~/org/todos.org" "General Tasks")
	   "* TODO [#B] %?\n:Created: %T\n "
	   :empty-lines 0)))

  (set-face-attribute 'org-block nil :background
					  (color-lighten-name (face-attribute 'default :background) 70))

  (setq org-src-block-faces '(("clojure" (:background (color-lighten-name
													   (face-attribute 'default :background) 70)))))

  (defvar org-src-fontify-natively 't)


  (custom-theme-set-faces
   'user
   `(org-level-4 ((t (:height 1.1))))
   `(org-level-3 ((t (:height 1.2))))
   `(org-level-2 ((t (:height 1.3))))
   `(org-level-1 ((t (:height 1.5))))
   `(org-document-title ((t (:height 1.6 :underline nil))))))

(use-package org-protocol
  :ensure nil)

(use-package evil-org
  :diminish evil-org-mode
  :after org
  :config
  (add-hook 'org-mode-hook 'evil-org-mode)
  (add-hook 'evil-org-mode-hook
			(lambda () (evil-org-set-key-theme))))

(require 'evil-org-agenda)
(evil-org-agenda-set-keys)

;;;;;;;; Lisp

(use-package lispyville
  :hook ((emacs-lisp-mode . lispyville-mode)
		 (clojure-mode . lispyville-mode)
		 (scheme-mode . lispyville-mode))
  :config
  (with-eval-after-load 'lispyville
	(lispyville-set-key-theme
	 '(operators
	   c-w
	   slurp/barf-cp
	   additional))))

(use-package slime
  :init
  (add-hook 'lisp-mode-hook (lambda () (slime-mode t)))
  (slime-setup '(slime-fancy slime-company slime-quicklisp slime-asdf))
  :config
  (setq inferior-lisp-program "sbcl"))

(use-package slime-company
  :after (slime company)
  :config
  (setq slime-company-completion 'fuzzy))

(use-package clojure-mode)
(use-package cider
  :config
  (add-hook 'clojure-mode-hook #'cider-mode)

  (general-define-key
   :states 'normal
   :keymaps 'clojure-mode-map
   :prefix ","
   "d" 'cider-doc
   "D" 'cider-clojuredocs)

  ;; Changes the startup command to enrich classpath with java libraries.
  ;; Should increase the amount of documentation support.
  (defvar cider-enrich-classpath 't)
  ;; Don't open a new buffer with the error
  (setq cider-show-error-buffer nil))

(use-package ob-clojure
  :config
  (setq org-babel-clojure-backend 'cider))

(use-package inf-clojure
  :init
  (defun cljs-node-repl ()
	(interactive)
	(inf-clojure "clj -M -m cljs.main -co build.edn -re node -r")))

(use-package paredit
  :config
  (add-hook 'clojure-mode-hook 'paredit-mode))

(use-package geiser)
(use-package geiser-guile)
(use-package geiser-chicken)
(use-package racket-mode)

;;;;;;;; Programming language specific

(use-package typescript-mode
  :mode "\\.tsx?$")

(use-package tide
  :ensure t
  :after (typescript-mode company flycheck)
  :hook ((typescript-mode . tide-setup)
		 (typescript-mode . tide-hl-identifier-mode)
		 (before-save . tide-format-before-save)))

(use-package plantuml-mode
  :custom
  (add-to-list 'auto-mode-alist '("\\.plantuml\\'" . plantuml-mode))
  (add-to-list 'org-src-lang-modes '("plantuml" . plantuml))
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((plantuml . t)))
  :config
  (setq plantuml-default-exec-mode 'exec))

(use-package sass-mode
  :mode "\\.sass?$"
  :custom
  (flycheck-mode nil)
  )

(use-package poly-ansible)
(use-package ansible-doc)
(use-package company-ansible)

(provide 'config)
;;; config.el ends here
