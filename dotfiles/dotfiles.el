(setq dotfiles-folder"~/configs/emacs-config")
(setq dotfiles-org-files '("emacs.org" "emacs-private.org"))

(defun dotfiles-tangle-org-file (&optional org-file)
  "Tangles ORG-FILE"
  (interactive "F")
  (message "Tangling File: %s" org-file)
  (let ((org-confirm-babel-evaluate nil)
	(message-log-max nil)
	(inhibit-message t))
    (org-babel-tangle-file (expand-file-name org-file dotfiles-folder))))

(defun dotfiles-tangle-org-files ()
  "Tangle all dotfiles."
  (interactive)
  (dolist (org-file dotfiles-org-files)
    (dotfiles-tangle-org-file org-file))
  (message "Dotfiles are up to date!"))

