;;;; package --- Summary: Packages for emacs init file

;;;; Commentary:

;; Emacs version >= 24 recommended

;;;; Code:

;; Manually loaded packages
;;(add-to-list 'load-path "~/.emacs.d/packages/")
;;(load "undo-tree-0.6.3.el")
;;(require 'undo-tree-0.6.3)

(require 'package)

;; Workaround for https://debbugs.gnu.org/cgi/bugreport.cgi?bug=34341
(defvar gnutls-algorithm-priority)
(setq gnutls-algorithm-priority "NORMAL:-VERS-TLS1.3")


(setq-default package-archives '(("gnu" . "https://elpa.gnu.org/packages/")
				 ("melpa" . "https://melpa.org/packages/")))

(package-initialize)

(unless package-archive-contents
  (package-refresh-contents))

(defun install-if-needed (package)
  "Install PACKAGE if not yet installed."
  (unless (package-installed-p package)
    (package-install package)))

;;;;;;;;;;;;;PACKAGES;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar to-install)
(setq to-install
      '(
	
	

	;; Color related
	rebecca-theme
	powerline ; Colorful bar :)
	rainbow-delimiters ; WE NEED MORE COLOR!
	color-identifiers-mode ; Colors variables with unique colors

	;; Improve clarity and speed
	aggressive-indent ; Single most important plugin
	ivy ; Completes me
	flycheck ; Check code while programming
	origami ; text folding
	evil
	smex ; better m-x, used by Ivy
	flx ; used by ivy


	magit ; Unused currently
	projectile ; Unused
	dired-sidebar ; Unused
	dired-single ; Unused


	
	company	; Code completion, generally better then AC
	company-c-headers
	company-web
	company-quickhelp
	counsel ; Complete all the things
	
	;;paredit ; May be useful when I program in Clojure or another Lisp again
	;;parinfer

	;; Ruby
	ruby-end ; Auto-insert an ending thing to block-stuff in Ruby
	inf-ruby ; Provides Ruby REPL
	ac-inf-ruby

	;; Ocaml
	tuareg
	merlin
	utop
	
	;;markup langs
	markdown-mode
	adoc-mode
	;; auctex ; Much nicer LaTeX support, large
	web-mode
	;; emmet-mode ; Don't really use this
	
	;; Other langs
	lua-mode
	company-lua
	csharp-mode
	sass-mode
        ))

(mapc 'install-if-needed to-install)

(defun tuareg-abbrev-hook ()
  "Prevents error in Tuareg."
  ())

(defvar flycheck-emacs-lisp-executable)
(set 'flycheck-emacs-lisp-executable "emacs")

(provide 'packages)
;;; packages.el ends here
