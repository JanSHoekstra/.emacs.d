;;;; package --- Summary: Keybindings for Emacs

;;;; Commentary:

;; Emacs version >= 24 recommended

;;;; Code:
(global-set-key "\C-cl" 'org-store-link)
(global-set-key "\C-ca" 'org-agenda)
(global-set-key "\C-cc" 'org-capture)
(global-set-key "\C-cb" 'org-switchb)
(global-set-key "\C-co" 'origami-open-node-recursively)
(global-set-key "\C-c'" 'origami-close-node)
(global-set-key "\C-c;" 'origami-open-all-nodes)
(global-set-key "\C-c[" 'origami-close-all-nodes)

;; Examples:
;;
;;
;;

(provide 'keybindings)
;;; keybindings.el ends here
