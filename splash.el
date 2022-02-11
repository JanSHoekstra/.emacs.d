;;; splosh --- A splash screen inspired by Nicolas P. Rougier's -*- lexical-binding: t; -*-

;;; Commentary:
;;; Have a look at the splash-screen I was inspired by here:
;;; https://github.com/rougier/emacs-splash/blob/master/splash-screen.el

;;; Code:

(require 'cl-extra)

(defun splosh-screen ()
  "Alt's splash screen."
  (interactive)

  ;; Unless there's file buffers open, open the splash screen.
  ;;(unless (cl-some 'buffer-file-name (buffer-list))
	;; Create 'splosh' screen.
	(let ((buffer (get-buffer-create "*splosh*")))
	  ;; Do things in the splosh screen.
	  (switch-to-buffer buffer)
	  ;; If buffer already exists, disable read only mode for a sec.
	  (when buffer-read-only (read-only-mode))
	  (erase-buffer)

	  ;; Hide all the things.
	  (setq mode-line-format nil)
	  (setq cursor-type nil)
	  ;; Set fill column width so (center-line) knows where to center to.
	  (setq fill-column (window-total-width nil 'floor))

	  ;; Vertical center
	  (insert-char ?\n (floor (/ (window-total-height nil 'floor) 2)))
	  ;; Insert cool splash text.
	  (insert (concat "Hello, " (or user-full-name (getenv "USER")) "!\n"))
	  (insert (concat "Welcome to GNU Emacs " (format "%d.%d" emacs-major-version emacs-minor-version) ".\n"))
	  (center-paragraph)
	  (insert-char ?\n)

	  ;; Set buffer to read-only, so user can't screw with splash.
	  ;;(read-only-mode)
	  ))
  ;;)

(setq inhibit-startup-screen t
	  inhibit-startup-message t
	  inhibit-startup-echo-area-message t )

;; Disable GNU Emacs help message.
(defun startup-echo-area-message () "." "")
;; Add splosh screen to startup.
(add-hook 'window-setup-hook 'splosh-screen)

(provide 'splash.el)
;;; splash.el ends here
