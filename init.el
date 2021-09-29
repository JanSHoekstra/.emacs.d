(setq lexical-binding t)
(setq gc-cons-threshold 100000000)
(setq custom-file "/tmp/emacs.custom.el")
(require 'package)
(setq package-archives '(("melpa" . "https://melpa.org/packages/")))
(package-initialize)
(unless (package-installed-p 'use-package)
  ;; only fetch the archives if you don't have use-package installed
  (package-refresh-contents)
  (package-install 'use-package))
(require 'use-package)
(setq use-package-always-ensure t)

(load "~/.emacs.d/config.el")
