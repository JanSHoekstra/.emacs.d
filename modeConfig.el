;;; package --- Summary: contains mode configuration.
;;; Commentary:
;;; Code:
(defmacro defset (js-name js-definition)
  "Defvar and setq in one.
JS-NAME is the variable and
JS-DEFINITION is the inputted value."
  (defvar js-name)
  (setq js-name js-definition))

(defun tuareg-abbrev-hook ()
  "Prevents error in Tuareg."
  ())

(defset 'flycheck-emacs-lisp-executable "emacs")

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

(defset company-idle-delay 0)
(defset company-minimum-prefix-length 1)
(defset company-selection-wrap-around t)
(company-tng-configure-default)

;; Disable annoying buffer pop-up.
(defset haskell-interactive-popup-errors nil)
(defset scheme-program-name "csi -:c")
(defset geiser-default-implementation "chicken")

;; C++
(defvar flycheck-clang-language-standard)
(add-hook 'c++-mode-hook (lambda () (setq flycheck-clang-language-standard "c++17")))

;; Also complete headers in C/C++, please.
(defvar company-backends)
(add-to-list 'company-backends 'company-c-headers)

(defset c-default-style "bsd")
(defset c-basic-offset 4)

(provide 'modeConfig)
;;; modeConfig.el ends here
