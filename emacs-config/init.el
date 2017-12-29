;;; init.el --- Init Emacs

;;; Commentary:
;; This files purpose is to load the use-library package and then load my-init.org


(require 'package)
;;; Code:

(setq package-enable-at-startup nil)

(add-to-list 'package-archives
	     '("melpa" . "http://melpa.org/packages/"))

(package-initialize)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(org-babel-load-file (expand-file-name "~/.emacs.d/my-init.org"))

(put 'dired-find-alternate-file 'disabled nil)

(provide 'init)

;;; init.el ends here
