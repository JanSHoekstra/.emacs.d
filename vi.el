;;; vi -- Summary
;;; Commentary:
;;; Code:
(evil-mode t)

;;; Make escape cancel everything
(defun minibuffer-keyboard-quit ()
  "Abort recursive edit.
In Delete Selection mode, if the mark is active, just deactivate it;
then it takes a second \\[keyboard-quit] to abort the minibuffer."
  (interactive)
  (if (and delete-selection-mode transient-mark-mode mark-active)
      (setq deactivate-mark  t)
    (when (get-buffer "*Completions*") (delete-windows-on "*Completions*"))
    (abort-recursive-edit)))

(defvar evil-normal-state-map)
(defvar evil-visual-state-map)
(define-key evil-normal-state-map [escape] 'keyboard-quit)
(define-key evil-visual-state-map [escape] 'keyboard-quit)
(define-key minibuffer-local-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-ns-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-completion-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-must-match-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-isearch-map [escape] 'minibuffer-keyboard-quit)
(global-set-key [escape] 'evil-exit-emacs-state)

;; Set cursor colors depending on mode
(when (display-graphic-p)
  (defvar evil-emacs-state-cursor)
  (defvar evil-normal-state-cursor)
  (defvar evil-visual-state-cursor)
  (defvar evil-insert-state-cursor)
  (defvar evil-replace-state-cursor)
  (defvar evil-operator-state-cursor)
  (setq evil-emacs-state-cursor '("red" box))
  (setq evil-normal-state-cursor '("green" box))
  (setq evil-visual-state-cursor '("light goldenrod" box))
  (setq evil-insert-state-cursor '("cyan" box))
  (setq evil-replace-state-cursor '("cyan" box))
  (setq evil-operator-state-cursor '("red" hollow))
  )

(provide 'vi.el)
;;; vi.el ends here
