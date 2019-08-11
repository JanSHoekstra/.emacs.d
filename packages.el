;;;; package --- Summary: Packages for emacs init file

;;;; Commentary:

;; Emacs version >= 24 recommended

;;;; Code:
(require 'package)

(setq-default package-archives '(("melpa" . "http://melpa.org/packages/")))

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
      '(aggressive-indent
	rebecca-theme
	powerline ; Colorful bar :)
	ivy ; Completes me
	magit ; Unused currently
	projectile ; Unused
	dired-sidebar ; Unused
	dired-single ; Unused
	flycheck ; Check code while programming
	origami ; text folding
	
	rainbow-delimiters ; WE NEED MORE COLOR!
	;;color-identifiers ; Colors variables with unique colors

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
	;;auctex ; Much nicer LaTeX support, large
	web-mode
	emmet-mode
	
	;; Other langs
	lua-mode
	company-lua
	csharp-mode ; C#
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
