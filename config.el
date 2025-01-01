;;; config.el --- Zoë Hoekstra's Emacs Setup  -*- lexical-binding: t; -*-

;;; Commentary:
;; This file includes my configuration, divided per package.

;;; Code:

;;;;;;;; Emacs

;; Please just shut up for a moment while starting up
;; Maybe replace with filtering if this proves a problem down the line:
;; https://www.emacswiki.org/emacs/EchoArea#h5o-3
(setq inhibit-message 1)
(run-at-time "10 sec" nil (lambda () (setq inhibit-message nil)))

;;;;;;;; Helpers
(defun set-exec-path-from-shell-PATH ()
	"Set up Emacs `exec-path` and PATH environment variable to match the shell.
This is particularly useful under Mac OS X and macOS, where GUI
apps are not started from a shell."
	(interactive)
	(let ((path-from-shell (replace-regexp-in-string
													"[ \t\n]*$" "" (shell-command-to-string
																					"$SHELL --login -c 'echo $PATH'"
																					))))
		(setenv "PATH" path-from-shell)
		(setq exec-path (split-string path-from-shell path-separator))))

(defun ensure-directory-exists (directory)
	"Ensure directory (as DIRECTORY) exists."
	(unless (file-directory-p directory)
		(make-directory directory t))
	(file-truename directory))

;;;;;;;; Base Emacs configuration

(use-package emacs
	:diminish auto-revert-mode
	:ensure nil
	:defer nil
	:config

	(setenv "PATH" (concat (getenv "PATH") ":/opt/homebrew/bin"))
	(if (not (eq system-type 'windows-nt))
			(progn
				(set-face-attribute 'default
														nil
														:family "iA Writer Mono S"
														:height 120)
				(set-face-attribute 'mode-line
														nil
														:family "iA Writer Quattro S"))
		(set-face-attribute 'default
												nil
												:family "Consolas"
												:height 140))

	;; Disable UI nonsense.
	(tool-bar-mode 0)
	(menu-bar-mode 0)
	(blink-cursor-mode 0)
	(tooltip-mode 0)
	(scroll-bar-mode 0)
	;; But add column number to the modeline please!
	(column-number-mode +1)

	(set-face-attribute 'mode-line nil :box nil)
	(set-face-attribute 'mode-line-inactive nil :box nil)

	;; Enable mouse in terminal
	(xterm-mouse-mode 1)

	(defun display-startup-echo-area-message ()
		()
		;;(message "")
		)

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
	 vc-follow-symlinks t
	 ;; I'm used to the key next to space behaving like 'alt'.
	 mac-command-modifier 'meta
	 ns-command-modifier 'meta)

	(setq-default indent-tabs-mode 0
								tab-width 2
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
					nil     ; should return nil if we're the current paste owner
				(shell-command-to-string "wl-paste -n | tr -d \r")))

		(setq interprogram-cut-function 'wl-copy)
		(setq interprogram-paste-function 'wl-paste))

	(when (eq system-type 'windows-nt)
		(setq user-emacs-directory (format  "C:/Users/%s/AppData/Roaming/.emacs.d" user-login-name))
		(setq default-directory (format "C:/Users/%s/" user-login-name))
		(setenv "HOME" (format "C:/Users/%s/" user-login-name)))

	(when (eq system-type 'darwin)
		(let ((shell "/opt/homebrew/bin/bash"))
			(setenv "SHELL" shell)
			(setenv "ESHELL" shell)
			(setq-default explicit-shell-file-name shell)))

	(unless (eq system-type 'windows-nt)
		(set-exec-path-from-shell-PATH)))

(use-package files
	:ensure nil
	:config
	(setq confirm-kill-processes nil
				create-lockfiles nil
				make-backup-files nil))
(use-package emacs
	:ensure nil
	:defer nil
	:config
	(add-to-list 'Info-directory-list "/opt/homebrew/share/info"))

;; Not stock Emacs, but load this asap
(use-package exec-path-from-shell
	:ensure t
	:defer nil
	:config
	(when (memq window-system '(mac ns x))
		(exec-path-from-shell-initialize)
		(when (eq system-type 'darwin)
			(exec-path-from-shell-copy-env "SSH_AGENT_PID")
			(exec-path-from-shell-copy-env "SSH_AUTH_SOCK"))))

;;;;;;;; Folds

(use-package hs-minor-mode
	:ensure nil
	:defer nil
	:hook (prog-mode . hs-minor-mode))

;;;;;;;; General

;; Enable evil vi bindings
(use-package evil
	:ensure t
	:defer nil
	:init
	(setq evil-undo-system 'undo-redo)
	(setq evil-want-minibuffer t)
	(evil-mode)
	(global-set-key (kbd "<escape>") 'keyboard-escape-quit))

;; Useful to remember keybinds
(use-package which-key
	:ensure t
	:diminish which-key-mode
	:defer nil
	:init
	(which-key-mode)
	;;(which-key-setup-minibuffer) ; used previously
	;;(which-key-setup-side-window-bottom) ; default
	:config
	(setq which-key-idle-delay 0.5))

(use-package general
	:ensure t
	:defer nil
	:init

	;; Global keybinds
	(general-define-key
	 :states '(normal visual motion)
	 :keymaps 'override
	 :prefix "SPC"
	 "1" 'delete-other-windows
	 "SPC" 'execute-extended-command
	 "c" 'comment-or-uncomment-region)
	(general-define-key
	 :states 'normal
	 :keymaps 'org-mode-map
	 :prefix ","
	 "t" 'org-todo)
	(general-define-key
	 :states 'normal
	 :keymaps 'elisp-mode
	 :prefix ","
	 "p" 'eval-defun))

;; Emacs native terminal, with decent behaviour and speed
(use-package eat
	:ensure t
	:defer t
	:hook (eat-mode . (lambda () (evil-emacs-state 1)))
	)

(use-package magit
	:ensure t
	:defer t)

(use-package dired-sidebar
	:ensure t
	:defer t
	:bind (("C-x C-n" . dired-sidebar-toggle-sidebar))
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

(use-package vertico
	:ensure t
	:defer nil
	:custom
	(vertico-count 10)
	(vertico-resize nil)
	:config
	(vertico-mode))

(use-package vertico-directory
	:ensure nil ; Included with vertico package
	:defer nil
	:after vertico
	:demand
	;; More convenient directory navigation commands
	:bind (:map vertico-map
							("RET"   . vertico-directory-enter)
							("DEL"   . vertico-directory-delete-char)
							("M-DEL" . vertico-directory-delete-word))
	;; Tidy shadowed file names
	:hook (rfn-eshadow-update-overlay . vertico-directory-tidy))

(use-package orderless
	:ensure t
	:defer nil
	:custom
	(completion-styles '(orderless))
	;;(orderless-matching-styles '(orderless-flex))
	(orderless-matching-styles '(orderless-literal)))

(use-package delight
	:ensure t
	:defer nil
	:config
	(delight 'eldoc-mode nil "eldoc")
	(delight 'lispyville-mode nil "lispyville")
	(delight 'aggressive-indent-mode nil "aggressive-indent")
	(delight 'evil-vimish-fold-mode nil "evil-vimish-fold")
	(delight 'centered-cursor-mode nil)
	(delight 'hs-minor-mode nil "hide-show")
	(delight 'company-mode nil "company"))

(use-package pinentry
	:ensure t
	:defer nil
	:config
	(setq epg-pinentry-mode 'loopback)
	(pinentry-start))

(use-package epa
	:ensure nil
	:defer 1
	:config
	(epa-file-enable))

(use-package pass
	:ensure t
	:defer t)

;;;;;;;; General Programming

(use-package whitespace
	:ensure nil
	:defer nil
	:config (defun clean-whitespace-in-prog-buffers ()
						(when (derived-mode-p major-mode)
							(whitespace-cleanup)))
	:hook (before-save . clean-whitespace-in-prog-buffers))

(use-package company
	:ensure t
	:hook ((prog-mode . company-mode)
				 (ielm . company-mode))
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

;; (use-package flycheck
;;	:ensure t
;;	:defer nil
;;	:config
;;	(global-flycheck-mode +1)
;;	;;(setq flycheck-check-syntax-automatically '(save mode-enable))
;;	(setq flycheck-highlighting-mode 'lines))

;; (use-package flycheck-inline
;;	:hook (flycheck-mode flycheck-inline-mode)
;;	:custom (flycheck-display-errors-delay 0))

;; (use-package flycheck-eglot
;;	:ensure t
;;	:after (flycheck eglot)
;;	:custom (flycheck-eglot-exclusive nil)
;;	:config
;;	(global-flycheck-eglot-mode 1))

(use-package centered-cursor-mode
	:ensure t
	:hook (prog-mode . centered-cursor-mode)
	:config (setq ccm-recenter-at-end-of-file t
								ccm-recenter-end-of-file t))

(use-package rainbow-delimiters
	:ensure t
	:defer t
	:bind ("\C-cr" . rainbow-delimiters-mode))

(use-package aggressive-indent
	:ensure t
	:config
	(global-set-key "\C-ci" 'aggressive-indent-mode)
	:hook ((prog-mode . aggressive-indent-mode)
				 (slime-repl-mode . aggressive-indent-mode)
				 (scheme-mode . aggressive-indent-mode)
				 (emacs-lisp-mode . aggressive-indent-mode)))

(use-package eglot
	:ensure nil
	:defer t
	:hook ((eglot-managed-mode . mp-eglot-eldoc)
				 (haskell-mode . eglot-ensure)
				 (clojure-mode . eglot-ensure)
				 (scala-mode . eglot-ensure)
				 (kotlin-mode . eglot-ensure))
	:bind ("\C-cl" . eglot)
	:custom
	;; shutdown language server after closing last file
	(eglot-autoshutdown t)
	;; allow edits without confirmation
	(eglot-confirm-server-initiated-edits nil)
	;; Set buffer size to 0 for to not impact performance
	;;((plist-get eglot-events-buffer-config :size) 0)
	;; disable stan
	(eglot-workspace-configuration
	 '((haskell
			(plugin
			 (stan
				(globalOn . :json-false)))))))

(use-package eldoc
	:ensure nil
	:defer t
	:hook ((emacs-lisp-mode . eldoc-mode)
				 (lisp-interaction-mode . eldoc-mode)
				 (ielm-mode . eldoc-mode))
	:config
	;; When eldoc buffer is open, don't show it in minibuffer anymore.
	(setq eldoc-echo-area-prefer-doc-buffer t)
	(setq eldoc-documentation-strategy 'eldoc-documentation-compose-eagerly)

	(add-to-list 'display-buffer-alist
							 '("^\\*eldoc for" display-buffer-at-bottom
								 (window-height . 10))))

;;;;;;;; Writing

(use-package markdown-mode
	:ensure t
	:defer t
	:mode "\\.md\\'"
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

;; TODO: Use-package-ify
(use-package org
	:ensure nil
	:defer nil
	:config
	;; Use the Quattro font only for org buffers
	(add-hook 'org-mode-hook
						(lambda ()
							(defvar buffer-face-mode-face
								'(:family "iA Writer Quattro V"))
							(buffer-face-mode)
							(visual-line-mode)
							(org-indent-mode)))

	;; Always be able to open the agenda through it's shortcut
	(global-set-key "\C-ca" 'org-agenda)
	(global-set-key "\C-cl" 'org-store-link)

	(general-define-key
	 :states '(normal)
	 :keymaps 'override
	 :prefix "SPC"
	 ;;"c" 'org-capture
	 "a" 'org-agenda
	 "o" 'org-open-at-point-global)

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
	(setq org-agenda-files (list "~/org/agenda"))

	;; Visually indent headings + content to their level
	;;(add-hook 'org-mode-hook 'org-indent-mode)

	;; Wrap lines so that things are easy to read
	;;(add-hook 'org-mode-hook 'visual-line-mode)

	;; Follow links in same buffer
	;;(setf (cdr (assoc 'file org-link-frame-setup)) 'find-file)

	;; Add all the events Emacs knows about
	(defvar org-agenda-include-diary nil)

	(setq org-tags-column 0)

	;; Disable confirmation before code execution
	(setq org-confirm-babel-evaluate nil)

	(setq org-cycle-separator-lines -1)

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
			 entry (file+olp+datetree "~/org/work-log.org")
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

	;;(set-face-attribute 'org-block nil :background
	;;(color-lighten-name (face-attribute 'default :background) 70))

	(setq org-src-block-faces '(("clojure" (:background (color-lighten-name
																											 (face-attribute 'default :background) 70)))))

	(defvar org-src-fontify-natively 't)

	(custom-theme-set-faces
	 'user
	 `(org-level-4 ((t (:height 1.1))))
	 `(org-level-3 ((t (:height 1.2))))
	 `(org-level-2 ((t (:height 1.3))))
	 `(org-level-1 ((t (:height 1.5)))) ;; :box (:color ,(face-attribute 'default :background) :line-width (0 . 10))))))
	 `(org-document-title ((t (:height 1.6 :underline nil)))))

	(font-lock-add-keywords 'org-mode
													'(("^ *\\([-]\\) "
														 (0 (prog1 () (compose-region (match-beginning 1) (match-end 1) "•")))))))

(use-package org-protocol
	:ensure nil
	:defer nil)

;; TODO: Use-package-ify
(use-package org-roam
	:ensure t
	:defer 2
	:config
	(ensure-directory-exists "~/org/roam")
	(setq org-roam-directory (file-truename "~/org/roam"))

	(general-define-key
	 :states '(normal)
	 :keymaps 'override
	 :prefix "SPC"
	 "r" 'org-roam-node-find)

	(general-define-key
	 :states '(normal)
	 :keymaps 'org-mode-map
	 :prefix ","
	 "r" 'org-roam-node-insert)

	(setq org-roam-capture-templates
				'(("d" "default" plain "%?"
					 :target (file+head "%<%Y%m%d%H%M%S>-${slug}.org"
															"#+title: ${title}\n")
					 :unnarrowed t)
					("w" "work" plain "%?"
					 :target (file+head "work/%<%Y%m%d%H%M%S>-${slug}.org"
															"#+title: ${title}\n")
					 :unnarrowed t)
					("P" "personal" plain "%?"
					 :target (file+head "personal/%<%Y%m%d%H%M%S>-${slug}.org"
															"#+title: ${title}\n")
					 :unnarrowed t)))

	(org-roam-db-autosync-mode))


;; TODO: Use-package-ify
(use-package org-roam-ui
	:after (org-roam)
	:config
	(setq org-roam-ui-sync-theme t
				org-roam-ui-follow t
				org-roam-ui-update-on-save t)

	(general-define-key
	 :states '(normal)
	 :keymaps 'override
	 :prefix "SPC"
	 "R" 'org-roam-ui-open))

(use-package evil-org
	:ensure t
	:defer t
	:diminish evil-org-mode
	:after org
	:hook ((org.mode . evil-org-mode)
				 (evil-org-mode . (lambda () (evil-org-set-key-theme)))))

(use-package org-jira
	:ensure t
	:defer t
	:config
	(setq jiralib-url "https://jira.ontwikkel.local")
	(setq org-jira-working-dir (ensure-directory-exists "~/.org/jira"))

	(setq jiralib-token
				(cons "Authorization"
							(concat "Bearer " (auth-source-pick-first-password
																 :host "jira.ontwikkel.local")))))

;; TODO: Use-package-ify
(use-package org-download
	:defer 5
	:config

	;; Drag-and-drop to `dired`
	(add-hook 'dired-mode-hook 'org-download-enable)
	(setq-default org-download-image-dir (ensure-directory-exists "~/org/img")))

;;;;;;;; Lisp

(use-package lispyville
	:ensure t
	:defer t
	:hook ((emacs-lisp-mode . lispyville-mode)
				 (clojure-mode . lispyville-mode)
				 (scheme-mode . lispyville-mode))
	:config

	;; https://github.com/noctuid/lispyville/issues/314
	;; https://github.com/abo-abo/lispy/issues/305
	(setq lispy-left "[([{]")
	(setq lispy-right "[])}]")

	(with-eval-after-load 'lispyville
		(lispyville-set-key-theme
		 '(operators
			 c-w
			 slurp/barf-cp
			 additional))))

(use-package elec-pair
	:ensure nil
	:hook (lispyville-mode . electric-pair-mode))

(use-package slime
	:ensure t
	:defer 20
	;;:hook ((lisp-mode . slime-mode))
	:mode "\\.cl\\'"
	:init
	;;(slime-setup '(slime-fancy slime-company slime-quicklisp slime-asdf))
	:config
	;;(setq inferior-lisp-program "sbcl"))
	)

(use-package slime-company
	:ensure t
	:defer t
	:after (slime company)
	:config
	(setq slime-company-completion 'fuzzy))

(use-package clojure-mode
	:ensure t
	:defer t
	:mode "\\.clj\\'")

(use-package cider
	:ensure t
	:defer t
	:hook ((clojure-mode . cider-mode))
	:config

	(general-define-key
	 :states 'normal
	 :keymaps 'clojure-mode-map
	 :prefix ","
	 "d" 'cider-doc
	 "D" 'cider-clojuredocs
	 "r" 'cider-run
	 "l" 'cider-load-buffer
	 "C" 'cider-jack-in
	 "p" 'cider-eval-defun-at-point
	 "e" 'cider-eval-last-sexp)

	;; Changes the startup command to enrich classpath with java libraries.
	;; Should increase the amount of documentation support.
	(defvar cider-enrich-classpath 't)
	;; Don't open a new buffer with the error
	(setq cider-show-error-buffer nil)
	(setq cider-eldoc-display-context-dependent-info t))

(use-package inf-clojure
	:after (clojure-mode)
	:init
	(defun cljs-node-repl ()
		(interactive)
		(inf-clojure "clj -M -m cljs.main -co build.edn -re node -r")))

(use-package paredit
	:hook ((lisp-mode . enable-paredit-mode)))

(use-package geiser
	:ensure t
	:defer t)
(use-package geiser-guile
	:ensure t
	:mode "\\.guile.scm\\'"
	:interpreter "guile")
(use-package geiser-chicken
	:ensure t
	:mode "\\.chicken.scm\\'"
	:interpreter "chicken")
(use-package racket-mode
	:ensure t
	:mode "\\.rkt\\'")

(use-package emacs
	:ensure nil
	:hook ((emacs-lisp-mode . (lambda () (evil-close-folds)))))

;;;;;;;; Programming language specific

(use-package treesit-auto
	:ensure t
	:defer nil
	:custom
	(treesit-auto-install 'prompt)
	(treesit-auto-add-to-auto-mode-alist 'all)
	(global-treesit-auto-mode))

(use-package plantuml-mode
	:ensure t
	:defer t
	:mode "\\.plantuml\\'"
	:custom
	(add-to-list 'auto-mode-alist '("\\.plantuml\\'" . plantuml-mode))
	(add-to-list 'org-src-lang-modes '("plantuml" . plantuml))
	;;(org-babel-do-load-languages
	;; 'org-babel-load-languages
	;; '((plantuml . t)))
	:config
	(setq plantuml-default-exec-mode 'exec))

(use-package sass-mode
	:mode "\\.sass?$"
	:custom
	(flycheck-mode nil))

(use-package poly-ansible
	:ensure t
	:defer t
	:mode "\\.yml\\'")

(use-package ansible-doc
	:ensure t
	:hook ((poly-ansible-mode . ansible-doc-mode)))

(use-package company-ansible
	:after (poly-ansible company))

(use-package kotlin-mode
	:mode "\\.kt\\'")

(use-package groovy-mode
	:mode "\\.groovy\\'"
	:config
	(setq groovy-indent-offset 2
				indent-tabs-mode 0
				tab-width 2
				truncate-lines 1
				truncate-partial-width-windows 1)
	(indent-tabs-mode nil))

(use-package rustic
	:mode "\\.rs\\'")

(use-package go-mode
	:mode "\\.go\\'")

(provide 'config)
;;; config.el ends here
