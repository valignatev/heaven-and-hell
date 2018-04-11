# Heaven and Hell [![Emacs](https://img.shields.io/badge/Emacs-24-8e44bd.svg)](https://www.gnu.org/software/emacs/) [![Emacs](https://img.shields.io/badge/Emacs-25-8e44bd.svg)](https://www.gnu.org/software/emacs/) [![Emacs](https://img.shields.io/badge/Emacs-26-8e44bd.svg)](https://www.gnu.org/software/emacs/)

![Logo](logo.jpg)

Heaven and Hell - emacs package for easy toggling of light/dark color themes.

## TLDR:

![Demo](demo.gif)

## Features

* Define your favorite light and dark themes
* Choose which one to run by default
* Switch between them at a single keypress
* Easily roll back to default emacs theme in case of messed faces

## Installation

Heaven and Hell is not yet in melpa. I'm working on that, but for now you have to install it from my repo.

### With `use-package`

TODO: write `use-package` example

### With `straight.el`

```emacs-lisp
(straight-use-package
 '(heaven-and-hell :type git :host github :repo "valignatev/heaven-and-hell"))
```

## Example configuration

```emacs-lisp
;; Default is 'light
(setq heaven-and-hell-theme-type 'dark)

;; Set preferred light and dark themes
;; default light is emacs default theme, default dark is wombat
(setq heaven-and-hell-themes
      '((light . #'tsdh-light)
	(dark . #'tsdh-dark)))

;; Add init-hook so heaven-and-hell can load your theme
(add-hook 'after-init-hook 'heaven-and-hell-init-hook)

;; Set keys to toggle theme and return to default emacs theme
(global-set-key (kbd "C-c <f6>") 'heaven-and-hell-load-default-emacs-theme)
(global-set-key (kbd "<f6>") 'heaven-and-hell-toggle-theme)
```

## Contribution

Feel free to open issue, submit pull request or share it anywhere in the Internet :)

## License

MIT
