;;; heaven-and-hell.el --- easy toggle light/dark themes -*- lexical-binding: t; -*-

;; Copyright (C) 2018 by Valentin Ignatev and contributors

;; Author: Valentin Ignatev <valentignatev@gmail.com>
;; URL: https://github.com/valignatev/heaven-and-hell
;; Version: 0.0.1
;; Package-Requires: ((emacs "24.4"))
;; Keywords: faces

;;; Commentary:
;; Light themes are easier on the eyes when sun is up.
;; But when it's dark around - you better to use dark theme.
;; This package makes process of switching between light and dark
;; theme as easy as hitting single keystroke.
;; Features:
;; * Define your favorite light and dark themes
;; * Choose which one to run by default
;; * Switch between them with a single keypress
;; * Easily roll back to default Emacs theme in case of messed faces
;;
;; Example configuration:
;; Default is 'light
;; (setq heaven-and-hell-theme-type 'dark)
;;
;; Set preferred light and dark themes (it can be a list of themes as well)
;; Default light is Emacs default theme, default dark is wombat
;; (setq heaven-and-hell-themes
;;       '((light . tsdh-light)
;; 	(dark . (tsdh-dark wombat))))
;;
;; Add init-hook so heaven-and-hell can load your theme
;; (add-hook 'after-init-hook 'heaven-and-hell-init-hook)
;;
;; Set keys to toggle theme and return to default Emacs theme
;; (global-set-key (kbd "C-c <f6>") 'heaven-and-hell-load-default-theme)
;; (global-set-key (kbd "<f6>") 'heaven-and-hell-toggle-theme)

;;; Code:

(defvar heaven-and-hell-themes
  '((light . nil)
    (dark . wombat))
  "Associate light and dark theme with this variable.
Theme can be the list.")

(defvar heaven-and-hell-theme-type 'light
  "Set default theme, either `light' or `dark'.")

(defun heaven-and-hell-themes-switch-to ()
  "Return themes which should be loaded according to current `heaven-and-hell-theme-type'."
  (cdr (assoc heaven-and-hell-theme-type heaven-and-hell-themes)))

(defun heaven-and-hell-clean-load-themes (theme-or-themes)
  "Load themes if they're not loaded yet and enable them cleanly.
Cleanly means that it disables all custom themes before enabling new ones.
THEME-OR-THEMES can be single theme or list of themes.
Themes will be loaded if they weren't loaded previously."
  (heaven-and-hell-load-default-theme)
  (when theme-or-themes
    (let ((themes (if (listp theme-or-themes) theme-or-themes `(,theme-or-themes))))
      (dolist (theme themes)
	(unless (custom-theme-p theme)
	  (load-theme theme)))
      (custom-set-variables `(custom-enabled-themes (quote ,themes))))))

;;;###autoload
(defun heaven-and-hell-toggle-theme ()
  "If `heaven-and-hell-theme-type' is `light' - load dark theme/s.
And vise-versa."
  (interactive)
  (if (eq heaven-and-hell-theme-type 'light)
      (setq heaven-and-hell-theme-type 'dark)
    (setq heaven-and-hell-theme-type 'light))
  (heaven-and-hell-clean-load-themes (heaven-and-hell-themes-switch-to)))

;;;###autoload
(defun heaven-and-hell-load-default-theme ()
  "Disable all custom themes e.g. load default Emacs theme."
  (interactive)
  (custom-set-variables '(custom-enabled-themes nil)))

;;;###autoload
(defun heaven-and-hell-init-hook ()
  "Add this to `after-init-hook' so it can load your theme/s of choice correctly."
  (interactive)
  (heaven-and-hell-clean-load-themes (heaven-and-hell-themes-switch-to)))

(provide 'heaven-and-hell)
;;; heaven-and-hell.el ends here
