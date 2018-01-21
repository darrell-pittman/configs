;;; init.el --- Init Emacs

;;; Commentary:
;; This files purpose is to load the use-library package and then load my-init.org


(require 'package)
;;; Code:

(setq package-enable-at-startup nil)

(add-to-list 'package-archives
	     '("melpa-stable" . "http://melpa-stable.milkbox.net/packages/") t)

(package-initialize)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(org-babel-load-file (expand-file-name "~/.emacs.d/my-init.org"))

;;; init.el ends here

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   (quote
    (yaml-mode dumb-jump cider expand-region undo-tree magit flycheck counsel-projectile projectile linum-relative multiple-cursors company counsel ace-window org-bullets which-key try solarized-theme use-package)))
 '(safe-local-variable-values (quote ((projectile-project-name . "rails")))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(aw-leading-char-face ((t (:inherit ace-jump-face-foreground :height 2.0)))))
