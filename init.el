;;;; package --- Summary: Main initialization for emacs

;;;; Commentary:

;; Emacs version >= 26.1 recommended

;;;;;;;;;;;;;INITIALIZATION;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Displays message about Emacs startup speed.
(add-hook 'emacs-startup-hook
	  (lambda ()
	    (message "Emacs ready in %s with %d garbage collections."
		     (format "%.2f seconds"
			     (float-time
			      (time-subtract after-init-time before-init-time)))
		     gcs-done)))

;; Disable special parsing while loading files in the init (tramp etc.)
(let ((file-name-handler-alist nil)))

;; reduce the frequency of garbage collection by making it happen on
;; each 5MB of allocated data (the default is on every 0.76MB)
;; Disable it on startup, however.
(setq gc-cons-threshold most-positive-fixnum
      gc-cons-percentage 0.6)

(add-hook 'after-init-hook
	  '(lambda ()
	     (setq gc-cons-threshold 5000000
		   gc-cons-percentage 0.1)))

;; warn when opening files bigger than 10MB
(setq large-file-warning-threshold 10000000)

;;;; Code:
(setq custom-file "~/.emacs.d/custom.el")
(load "~/.emacs.d/packages.el")
(load custom-file)
(load "~/.emacs.d/vi.el")
(load "~/.emacs.d/keybindings.el")
(load "~/.emacs.d/modeConfig.el")
(load "~/.emacs.d/org.el")

(defmacro defset (js-name js-definition)
  "Defvar and setq in one.
JS-NAME is the variable and
JS-DEFINITION is the inputted value."
  (defvar js-name)
  (setq js-name js-definition))

;;;;;;;;;;;;;GLOBAL MODES;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(rainbow-delimiters-mode-enable) ;; We need to know what belongs to who
(global-aggressive-indent-mode) ;; Force those indents hard!
(powerline-center-theme) ;; bar all the way, at the bottom of the screen 0.0
(global-color-identifiers-mode) ;; Give all the variables COLOR.
(ivy-mode 1) ;; autocomplete the M-x thingybar stuff
(ido-mode 1) ;; Even nicer autocomplete stuff
(global-hl-line-mode) ; Highlight cursor line
(electric-pair-mode 1) ; Auto-insert pairs everywhere
(global-company-mode) ; Autocomplete everywhere
(global-origami-mode) ; Folding everywhere
(global-display-line-numbers-mode) ; Line numbers!
(setq display-line-numbers 'relative) ; For easy jumping with evil
;;;;;;;;;;;;;VARIABLES;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;; General UI

(tool-bar-mode 0)                              ; Don't need a toolbar...
(menu-bar-mode 0)                              ; Let's go clean
(scroll-bar-mode 0)                            ; Let's go ultra clean
(define-key menu-bar-tools-menu [games] 0)     ; Remove games menu
(setq inhibit-startup-message t)               ; No startup message
(blink-cursor-mode 0)                        ; I'm already agitated enough
(mouse-wheel-mode t)                           ; Mouse wheel enabled
(setq inhibit-compacting-font-caches t)        ; Prevents font caches from being gc'd
(setq focus-follows-mouse t)                   ;
(setq mouse-autoselect-window t)               ; Makes mousefocus act like in Linux X.org
(setq ring-bell-function 'ignore)              ; Do not make annoying noises

;;;;;; I'm a European, so...
(defset european-calendar-style 't)
(defset calendar-week-start-day 1)
(setq ps-paper-type 'a4)                       ; Specify printing format

;;;;;; Files 'n stuff
(setq auto-save-timeout 60)                    ; Autosave every minute
(setq read-file-name-completion-ignore-case 't); Ignore case when completing file names

;;;;;; Tabs, spaces, indents, lines, parentheses, etc.
(setq indent-tabs-mode 0)
(setq-default c-basic-offset 4)                ; use 4 spaces as indentation
(show-paren-mode t)                            ; Highlight parenthesis pairs
(setq blink-matching-paren-distance 0)         ; Blinking parenthesis = no
(defset show-paren-style 'expression)          ; Highlight text between parentheses

;; No stupid backup/temporary files in every folder, but in a dedicated one
(setq
 version-control t
 kept-new-versions 6
 kept-old-versions 2
 backup-by-copying t
 backup-directory-alist '(("." . "~/.emacs.d/saves"))
 delete-old-versions t)

;; Font!
(set-face-attribute 'default nil :family "Fira Code" :height 120)

;;;;;;;;;;;;;FUNCTIONS;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun vsplit-last-buffer ()
  "When opening a new split, open previous buffer.
Instead of 2 identical ones.  Vertically."
  (interactive)
  (split-window-vertically)
  (other-window 1 nil)
  (switch-to-next-buffer))

(defun hsplit-last-buffer ()
  "When opening a new split, open previous buffer.
Instead of 2 identical ones.  Horizontally."
  (interactive)
  (split-window-horizontally)
  (other-window 1 nil)
  (switch-to-next-buffer))

(global-set-key (kbd "C-x 2") 'vsplit-last-buffer)
(global-set-key (kbd "C-x 3") 'hsplit-last-buffer)

;; Weird theme workaround (for running in server mode)
(defvar my:theme 'rebecca)
(defvar my:theme-window-loaded nil)
(defvar my:theme-terminal-loaded nil)

(if (daemonp)
    (add-hook 'after-make-frame-functions(lambda (frame)
					   (select-frame frame)
					   (if (window-system frame)
					       (unless my:theme-window-loaded
						 (if my:theme-terminal-loaded
						     (enable-theme my:theme)
						   (load-theme my:theme t))
						 (setq my:theme-window-loaded t))
					     (unless my:theme-terminal-loaded
					       (if my:theme-window-loaded
						   (enable-theme my:theme)
						 (load-theme my:theme t))
					       (setq my:theme-terminal-loaded t)))))

  (progn
    (load-theme my:theme t)
    (if (display-graphic-p)
        (setq my:theme-window-loaded t)
      (setq my:theme-terminal-loaded t))))

;; Ivy, Counsel, Swiper Setup ;;;;;;;;;;;;;;;;;;;;;;;;
(ivy-mode 1) ;; Turn on ivy by default
(defset ivy-use-virtual-buffers t)  ;; adds virtual buffers (bookmarks etc.)
(setq enable-recursive-minibuffers t) ;; no idea, but recommended by project maintainer
(defset ivy-re-builders-alist '((ivy-switch-buffer . ivy--regex-ignore-order)
				(t . ivy--regex-fuzzy))) ;; ignore order of words in ivy
(defset ivy-count-format "(%d/%d) ")  ;; changes the format of the number of results
(defset ivy-initial-inputs-alist nil)
(defset ivy-height 16)

(global-set-key (kbd "C-s") 'swiper)  ;; replaces i-search with swiper
(global-set-key (kbd "M-x") 'counsel-M-x) ;; Gives M-x command counsel features
(global-set-key (kbd "C-x C-f") 'counsel-find-file) ;; gives C-x C-f counsel features
(global-set-key (kbd "<f1> f") 'counsel-describe-function)
(global-set-key (kbd "<f1> v") 'counsel-describe-variable)
(global-set-key (kbd "<f1> l") 'counsel-find-library)
(global-set-key (kbd "<f2> i") 'counsel-info-lookup-symbol)
(global-set-key (kbd "<f2> u") 'counsel-unicode-char)
(global-set-key (kbd "C-c C-r") 'ivy-resume)

(global-set-key (kbd "C-c g") 'counsel-git)
(global-set-key (kbd "C-c j") 'counsel-git-grep)
(global-set-key (kbd "C-c k") 'counsel-ag) ;; add counsel/ivy features to ag package
(global-set-key (kbd "C-x l") 'counsel-locate)
(global-set-key (kbd "C-S-o") 'counsel-rhythmbox)
;;(define-key read-expression-map (kbd "C-r") 'counsel-expression-history)

;;set action options during execution of counsel-find-file
;; replace "frame" with window to open in new window
(ivy-set-actions
 'counsel-find-file
 '(("j" find-file-other-frame "other frame")
   ("b" counsel-find-file-cd-bookmark-action "cd bookmark")
   ("x" counsel-find-file-extern "open externally")
   ("d" delete-file "delete")
   ("r" counsel-find-file-as-root "open as root")))

;; set actions when running C-x b
;; replace "frame" with window to open in new window
(ivy-set-actions
 'ivy-switch-buffer
 '(("j" switch-to-buffer-other-frame "other frame")
   ("k" kill-buffer "kill")
   ("r" ivy--rename-buffer-action "rename")))

;; Theming
(load-theme 'rebecca t)

(set-face-foreground 'rainbow-delimiters-depth-1-face "deep sky blue")
(set-face-foreground 'rainbow-delimiters-depth-2-face "magenta1")
(set-face-foreground 'rainbow-delimiters-depth-3-face "turquoise1")
(set-face-foreground 'rainbow-delimiters-depth-4-face "dark orange")
(defset rainbow-delimiters-max-face-count 4)

(global-set-key '[f5] 'org-capture)
(global-set-key '[f6] (lambda () (interactive)(find-file "~/org/inbox.org")))
;;(global-set-key '[f7] 'org-capture)
;;(global-set-key '[f8] 'org-capture)
;;(global-set-key '[f9] 'org-capture)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;(custom-set-variables
;; custom-set-variables was added by Custom.
;; If you edit it by hand, you could mess it up, so be careful.
;; Your init file should contain only one such instance.
;; If there is more than one, they won't work right.
;; '(ansi-color-faces-vector
;;   [default default default italic underline success warning error])
;; '(font-latex-fontify-script nil)
;; )
;; Load org file
(find-file "~/org/todo.org")

(provide 'init)
;;; init.el ends here
