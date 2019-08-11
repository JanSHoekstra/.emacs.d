;;; package --- Summary: contains mode configuration.
;;; Commentary:
;;; Code:
(add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))
(add-to-list 'auto-mode-alist '("\\.html\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.adoc\\'" . adoc-mode))

(remove-hook 'adoc-mode 'font-lock-mode)
(add-hook 'prog-mode-hook 'rainbow-delimiters-mode)
(add-hook 'prog-mode-hook 'company-mode)
(add-hook 'prog-mode-hook 'flycheck-mode)
(add-hook 'emacs-lisp-mode 'prettify-symbols-mode)
(add-hook 'tuareg-mode-hook 'origami-mode)
(add-hook 'tuareg-mode-hook 'merlin-mode)

(defvar company-idle-delay)
(defvar company-minimum-prefix-length)
(defvar company-selection-wrap-around)
(setq company-idle-delay 0)
(setq company-minimum-prefix-length 1)
(setq company-selection-wrap-around t)
(company-tng-configure-default)



(provide 'modeConfig)
;;; modeConfig.el ends here
