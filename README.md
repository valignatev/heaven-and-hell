# Heaven and Hell [![Emacs](https://img.shields.io/badge/Emacs-24-8e44bd.svg)](https://www.gnu.org/software/emacs/) [![Emacs](https://img.shields.io/badge/Emacs-25-8e44bd.svg)](https://www.gnu.org/software/emacs/) [![Emacs](https://img.shields.io/badge/Emacs-26-8e44bd.svg)](https://www.gnu.org/software/emacs/)    [![MELPA](https://melpa.org/packages/heaven-and-hell-badge.svg)](https://melpa.org/#/heaven-and-hell) [![MELPA Stable](https://stable.melpa.org/packages/heaven-and-hell-badge.svg)](https://stable.melpa.org/#/heaven-and-hell)

![Logo](logo.jpg)

Heaven and Hell - emacs package for easy toggling of light/dark color themes.

## TLDR:

![Demo](demo.gif)

## Why?

I constantly switch light and dark theme.
When it's sunny outside, or I work in a very light room - I use the light theme.
Otherwise, I prefer dark one.
So I wrote a bunch of helpers which helped me with this and one day I decided to make a package out of it.
Hope it'll be useful to anybody else. Cheers!

## Features

* Define your favorite light and dark themes
* Choose which one to run by default
* Switch between them with a single keypress
* Easily roll back to default emacs theme in case of messed faces

## Installation

`heaven-and-hell` is available in [Melpa](https://melpa.org/#/heaven-and-hell).

### With `use-package`

With `use-package` (including example config):
```emacs-lisp
(use-package heaven-and-hell
  :ensure t
  :init
  (setq heaven-and-hell-theme-type 'dark) ;; Omit to use light by default
  (setq heaven-and-hell-themes
        '((light . tsdh-light)
          (dark . tsdh-dark))) ;; Themes can be the list: (dark . (tsdh-dark wombat))
  :hook (after-init . heaven-and-hell-init-hook)
  :bind (("C-c <f6>" . heaven-and-hell-load-default-theme)
         ("<f6>" . heaven-and-hell-toggle-theme)))
```

### With `straight.el`

```emacs-lisp
(straight-use-package 'heaven-and-hell)
```

### Manually

First clone the repo:

```shell
git clone https://github.com/valignatev/heaven-and-hell.git your/site/elisp/path
```

Then add this in your `init.el`:

```emacs-lisp
(require 'heaven-and-hell "your/site/elisp/path/heaven-and-hell/heaven-and-hell.el")
```

## Example configuration

For `use-package` configuration look above in installation section

```emacs-lisp
;; Default is 'light
(setq heaven-and-hell-theme-type 'dark)

;; Set preferred light and dark themes
;; default light is emacs default theme, default dark is wombat
;; Themes can be the list: (dark . (tsdh-dark tango-dark))
(setq heaven-and-hell-themes
      '((light . tsdh-light)
        (dark . tsdh-dark)))

;; Add init-hook so heaven-and-hell can load your theme
(add-hook 'after-init-hook 'heaven-and-hell-init-hook)

;; Set keys to toggle theme and return to default emacs theme
(global-set-key (kbd "C-c <f6>") 'heaven-and-hell-load-default-theme)
(global-set-key (kbd "<f6>") 'heaven-and-hell-toggle-theme)
```

## Contribution

Have trouble with installation? Found a bug? Want a new feature? Feel free to [open an issue](https://github.com/valignatev/heaven-and-hell/issues/new)!

Want to fix a bug or add something new yourself? Feel free to submit pull request!

Also, don't hesitate to share it anywhere in the Internet :)

## License

MIT
