;;; package --- Summary: contains mode configuration.
;;; Commentary:
;;; Code:
(defun tuareg-abbrev-hook ()
  "Prevents error in Tuareg."
  ())

(defvar flycheck-emacs-lisp-executable "emacs")

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

(defvar company-idle-delay 0)
(defvar company-minimum-prefix-length 1)
(defvar company-selection-wrap-around t)
(company-tng-configure-default)

;; Disable annoying buffer pop-up.
(defvar haskell-interactive-popup-errors nil)
(defvar scheme-program-name "csi -:c")
(defvar geiser-default-implementation "chicken")

;; C++
(add-hook 'c++-mode-hook (lambda () (defvar flycheck-clang-language-standard "c++17")))

;; Also complete headers in C/C++, please.
(defvar company-backends)
(add-to-list 'company-backends 'company-c-headers)

;;(defvar c-default-style '(("c++" . "linux")
;;(java-mode . "java")
;;(awk-mode . "awk")
;;(other . "linux")))
;;(defvar c-basic-offset 4)

(provide 'modeConfig)
;;; modeConfig.el ends here
