;;;; package --- Summary: Org-mode specific configuration

;;;; Commentary:

;; Emacs version >= 24 recommended

;;;; Code:
(defvar org-export-with-section-numbers)
(setq org-export-with-section-numbers nil)

;;;; Agenda
(defvar org-agenda-files)
(setq org-agenda-files '("~/.emacs.d/todo.org"))

(org-agenda nil "a") ; Start agenda files at Emacs start

(defvar org-startup-folded)
(setq org-startup-folded 0)
(defvar org-agenda-span)
(setq org-agenda-span 10)

;;(defvar org-latex-toc-command)
;;(setq org-latex-toc-command '/tableofcontents /newpage') ; Define this later

;;; org.el ends here
