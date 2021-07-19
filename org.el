;;;; package --- Summary: Org-mode specific configuration

;;;; Commentary:

;; Emacs version >= 24 recommended

;;;; Code:
(defmacro defset (js-name js-definition)
  "Defvar and setq in one.
JS-NAME is the variable and
JS-DEFINITION is the inputted value."
  (defvar js-name)
  (setq js-name js-definition))

(with-eval-after-load 'org
  (defset org-export-with-section-numbers nil))

;; Reflow text when resizing emacs window
(add-hook 'org-mode-hook 'visual-line-mode)

;;;; Agenda
(defset org-agenda-files '("~/org/todo.org"))
;; Start agenda files at Emacs start
;(org-agenda nil "a")
;; Start agenda on today
(defset org-agenda-start-on-weekday nil)
(defset org-agenda-span 10)

(defset org-hide-emphasis-markers t)
(defset org-startup-folded 0)
(defset org-export-with-section-numbers nil)
(defset org-todo-keyword-faces
  '(("TODO" . (:foreground "light goldenrod" :background nil))
    ("DOING" . (:foreground "cyan1" :background nil))
    ("NOTE" . (:foreground "gray" :background nil))))

(defset org-capture-templates
  '(("t" "todo" entry (file "~/org/inbox.org") "* TODO %?\n%a\n")
    ("n" "note" entry (file "~/org/notes.org") "** NOTE %? \n%a\n")))

(defun js-mark-as-project ()
  "Turn current heading into a project."
  (interactive)
  (org-toggle-tag "project" 'on)
  (org-set-property "COOKIE_DATA" "todo recursive")
  (org-back-to-heading t)
  (let* ((title (nth 4 (org-heading-components)))
	 (keyword (nth 2 (org-heading-components))))
    (when (and (bound-and-true-p keyword) (string-prefix-p "[" title))
      (message "TODO keyword and progress indicator found"))
    (when (and (not (bound-and-true-p keyword)) (string-prefix-p "[" title))
      (message "no TODO keyword but progress indicator found")
      (forward-whitespace 1)
      (insert "NEXT "))
    (when (and (not (bound-and-true-p keyword)) (not (string-prefix-p "[" title)))
      (message "no TODO keyword and no progress indicator found")
      (forward-whitespace 1)
      (insert "NEXT [/] "))
    (when (and (bound-and-true-p keyword) (not (string-prefix-p "[" title)))
      (message "TODO keyword but no progress indicator found")
      (forward-whitespace 2)
      (insert "[/] "))))

;;(defvar org-latex-toc-command)
;;(setq org-latex-toc-command '/tableofcontents /newpage') ; Define this later

;;; org.el ends here
