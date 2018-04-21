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
  (heaven-and-hell-load-default-emacs-theme)
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
(defun heaven-and-hell-load-default-emacs-theme ()
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
