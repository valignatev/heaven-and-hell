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
;; Set preferred light and dark themes
;; Default light is Emacs default theme, default dark is wombat
;; (setq heaven-and-hell-themes
;;       '((light . 'tsdh-light)
;; 	(dark . 'tsdh-dark)))
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
    (dark . 'wombat))
  "Associate light and dark theme with this variable.")

(defvar heaven-and-hell-theme-type 'light
  "Set default theme, either `light' or `dark'.")

(defun heaven-and-hell-theme-switch-to ()
  "Return theme which should be loaded according to current `heaven-and-hell-theme-type'."
  (eval (cdr (assoc heaven-and-hell-theme-type heaven-and-hell-themes))))

(defun heaven-and-hell-clean-load-theme (theme)
  "Same as `load-theme' but reset all theme settings before loading new theme.
THEME - theme to load or nil, nil means reset all themes and fallback to Emacs default"
  (heaven-and-hell-load-default-theme)
  (when theme
    (load-theme theme t)))

;;;###autoload
(defun heaven-and-hell-toggle-theme ()
  "If `heaven-and-hell-theme-type' is `light' - load dark theme.
And vise-versa."
  (interactive)
  (if (eq heaven-and-hell-theme-type 'light)
      (setq heaven-and-hell-theme-type 'dark)
    (setq heaven-and-hell-theme-type 'light))
  (heaven-and-hell-clean-load-theme (heaven-and-hell-theme-switch-to)))

;;;###autoload
(defun heaven-and-hell-load-default-theme ()
  "Reset all theme settings and e.g. load default Emacs theme."
  (interactive)
  (mapcar #'disable-theme custom-enabled-themes))

;;;###autoload
(defun heaven-and-hell-init-hook ()
  "Add this to `after-init-hook' so it can load your theme of choice correctly."
  (interactive)
  (heaven-and-hell-clean-load-theme (heaven-and-hell-theme-switch-to)))

(provide 'heaven-and-hell)
;;; heaven-and-hell.el ends here
