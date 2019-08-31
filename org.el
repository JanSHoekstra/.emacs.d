;;;; package --- Summary: Org-mode specific configuration

;;;; Commentary:

;; Emacs version >= 24 recommended

;;;; Code:
(defvar org-export-with-section-numbers)
(setq org-export-with-section-numbers nil)

;;;; Agenda
(defvar org-agenda-files)
(setq org-agenda-files '("~/org/todo.org"))

(org-agenda nil "a") ; Start agenda files at Emacs start

(defvar org-startup-folded)
(setq org-startup-folded 0)
(defvar org-agenda-span)
(setq org-agenda-span 10)
(defvar org-agenda-start-on-weekday)
;; Start agenda on today
(setq org-agenda-start-on-weekday nil)

(defvar org-todo-keyword-faces)
(setq org-todo-keyword-faces '(("TODO" . (:foreground "light goldenrod" :background nil))
			       ("DOING" . (:foreground "cyan1" :background nil))
			       ))


;;(defvar org-latex-toc-command)
;;(setq org-latex-toc-command '/tableofcontents /newpage') ; Define this later

;;; org.el ends here
