;;;; package --- Summary: Keybindings for Emacs

;;;; Commentary:

;; Emacs version >= 24 recommended

;;;; Code:
(global-set-key "\C-cl" 'org-store-link)
(global-set-key "\C-ca" 'org-agenda)
(global-set-key "\C-cc" 'org-capture)
(global-set-key "\C-cb" 'org-switchb)


;; This key is now mine!
(global-unset-key "\C-z")
(global-set-key (kbd "C-z C-z") 'package-install)
;;(global-set-key "\C-zo" (lambda () (interactive) (origami-open-node-recursively)))
;;(global-set-key "\C-z'" (lambda () (interactive) (origami-close-node)))
;;(global-set-key "\C-z;" (lambda () (interactive) (origami-open-all-nodes)))
;;(global-set-key "\C-z[" (lambda () (interactive) (origami-close-all-nodes)))
;; Examples:
;;
;;
;;

(provide 'keybindings)
;;; keybindings.el ends here
