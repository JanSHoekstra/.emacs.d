;;; package --- Summary

;;; Commentary:

;;; Code:

(use-package emacs
  :ensure nil
  :config
  (tool-bar-mode 0)
  (menu-bar-mode 0)
  (blink-cursor-mode 0)

  (setq-default indent-tabs-mode 0
				tab-width 4
				truncate-lines 1
				truncate-partial-width-windows 1)

  (setq ring-bell-function 'ignore)
  )

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

;; Delete intermediate buffers when navigating through dired.
(use-package dired
  :ensure nil
  :config
  (setq delete-by-moving-to-trash t)
  (eval-after-load "dired"
	#'(lambda ()
		(put 'dired-find-alternate-file 'disabled nil)
		(define-key dired-mode-map (kbd "RET") #'dired-find-alternate-file))))

(use-package ido
  :config
  (ido-mode +1)
  (setq ido-everywhere t
		ido-enable-flex-matching t))

(use-package ido-vertical-mode
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

(use-package challenger-deep-theme :config (load-theme 'challenger-deep 't))

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
