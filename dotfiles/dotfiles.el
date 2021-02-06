;;; dotfiles.el -- Simple package to tangle org config files.
;;; Commentary:
;;; Code:

(require 'org)

(defcustom dotfiles-folder "~/configs/emacs-config"
  "Folder where 'dotfiles and 'org-mode configuration files are stored."
  :type 'string
  :group 'dotfiles)

(defcustom dotfiles-org-files '("emacs.org" "emacs-private.org")
  "List of 'org-mode files in the `dotfiles-folder' that contains files that should be tangled."
  :type '(list string)
  :group 'dotfiles)

(defun dotfiles-tangle-org-file (&optional org-file)
  "Tangles ORG-FILE."
  (interactive "F")
  (message "Tangling Org File: %s" org-file)
  (let ((org-confirm-babel-evaluate nil)
	(message-log-max nil)
	(inhibit-message t))
    (org-babel-tangle-file (expand-file-name org-file dotfiles-folder))))

(defun dotfiles-tangle-org-files ()
  "Tangle all dotfiles specified in dotfiles-org-files."
  (interactive)
  (dolist (org-file dotfiles-org-files)
    (dotfiles-tangle-org-file org-file))
  (message "Dotfiles are up to date!"))

;;; dotfiles.el ends here
