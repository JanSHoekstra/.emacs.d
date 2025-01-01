;; Emacs 28+: Modus themes are now be built-in. Load it asap to minimize flashing
(load-theme 'modus-vivendi-tinted 't)
(setq lexical-binding t)
(setq gc-cons-threshold 100000000)
(setq custom-file "/tmp/emacs.custom.el")
(require 'package)
(setq package-archives '(("melpa" . "https://melpa.org/packages/")
												 ("gnu" . "https://elpa.gnu.org/packages/")
												 ("nongnu" . "https://elpa.nongnu.org/nongnu/")))
(package-initialize)
(unless (package-installed-p 'use-package)
	;; only fetch the archives if you don't have use-package installed
	(package-refresh-contents)
	(package-install 'use-package))
(require 'use-package)
(setq use-package-always-ensure t)
;; In case Emacs startup time is slow
;;(setopt use-package-verbose t)
;;(setopt use-package-compute-statistics t)

(load "~/.emacs.d/config.el")
