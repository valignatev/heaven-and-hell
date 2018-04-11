(defvar heaven-and-hell-themes
  '((light . nil)
    (dark . #'wombat)))

(defvar heaven-and-hell-theme-type 'light)

(defun heaven-and-hell-theme-switch-to ()
  (eval (cdr (assoc heaven-and-hell-theme-type heaven-and-hell-themes))))

(defun heaven-and-hell-clean-load-theme (theme)
  (heaven-and-hell-load-default-emacs-theme)
  (when theme
    (load-theme theme t)))

(defun heaven-and-hell-toggle-theme ()
  (interactive)
  (if (eq heaven-and-hell-theme-type 'light)
      (setq heaven-and-hell-theme-type 'dark)
    (setq heaven-and-hell-theme-type 'light))
  (heaven-and-hell-clean-load-theme (heaven-and-hell-theme-switch-to)))

(defun heaven-and-hell-load-default-emacs-theme ()
  (interactive)
  (mapcar #'disable-theme custom-enabled-themes))

(defun heaven-and-hell-init-hook ()
  (interactive)
  (heaven-and-hell-clean-load-theme (heaven-and-hell-theme-switch-to) t))

(global-set-key (kbd "<f6>") 'heaven-and-hell-toggle-theme)

;; Reset to default emacs theme
(global-set-key (kbd "C-c <f6>") 'heaven-and-hell-load-default-emacs-theme)

(add-hook 'after-init-hook 'heaven-and-hell-init-hook)
