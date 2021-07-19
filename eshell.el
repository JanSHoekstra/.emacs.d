;;;; package --- Summary: Eshell related aliases and functions
;;;; Commentary:
;; nothing.
;;; Code:

(defalias 'shell 'eshell)

(defun eshell/emacs (file)
  "Intercept Emacs in eshell.  Open FILE directly."
  (find-file file))

(defun eshell/vim (file)
  "Intercept Vim in eshell.  Open FILE directly."
  (find-file file))

(defun eshell/vi (file)
  "Intercept Vi in eshell.  Open FILE directly."
  (find-file file))

(defun eshell/nvim (file)
  "Intercept Vi in eshell.  Open FILE directly."
  (find-file file))


(provide 'eshell)
;;; eshell.el ends here
