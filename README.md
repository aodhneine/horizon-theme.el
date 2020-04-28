# Horizon
![such-an-awesome-theme](./screenshots/screenshot-2.png)

A [Horizon](https://horizontheme.netlify.com) theme by [jolaleye](https://github.com/jolaleye), ported from Visual Studio Code to Emacs by aodhneine.

## Installation
### Automated
This package is available on MELPA, you can install it using any emacs package manager that supports it.

If you use `package.el`, default package manager for Emacs, include this in your init file:
``` emacs-lisp
(require 'package)
(add-to-list 'package-archives
             '("melpa" . "http://melpa.milkbox.net/packages/") t)
```
Then you can install `use-package` and use it to install this theme:

```emacs-lisp
(use-package horizon-theme)
```

If you use `straight.el`, bootstrap it by following official instructions, and then put the following lines in init file:

```emacs-lisp
(straight-use-package 'use-package)
(setq straight-use-package-by-default t)

(use-package horizon-theme)
```

### Manual
If you don't want to use MELPA, you can also install this theme manually. Just clone this repository:
``` shell
git clone https://github.com/aodhneine/horizon-theme.el.git
```
and then add the following lines to your init file:
``` emacs-lisp
(add-to-list 'custom-theme-load-path "path/to/horizon-theme.el")
(load-theme 'horizon t)
```

## Contribution
You can contribute to this theme by cloning the repo, making your changes and then creating pull requests. Because of lack of time, I only added support for faces which I use, so many may be lacking. Thank you very much for your interest in this project. <3
