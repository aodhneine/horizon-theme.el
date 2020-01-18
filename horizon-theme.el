;;; horizon-theme.el ---  A beautifully warm dual theme for Emacs -*- lexical-binding: t; -*-

;; This file is not part of GNU Emacs.
;; Copyright (C) 2019-2020 Aodnait Étaín
;; Authors: Aodnait Étaín <aodhneine@tuta.io>
;; Version: 0.0.3
;; URL: https://github.com/aodhneine/horizon-theme.el
;; Package-Requires: ((emacs "27"))

;;; Commentary:

;;; Credits:
;; This theme is based on its namesake, Horizon theme for Visual Studio Code,
;; which can be find here: https://horizontheme.netlify.com/.
;; Huge thanks to rexim, https://github.com/rexim/gruber-darker-theme for
;; creating simple theme on which I based this implementation. <3

;;; License:
;; This project is licensed under MIT License.

;;; Code:

(deftheme horizon
  "A beautifully warm dual theme for Emacs")

(let* (;; syntax
       (lavender "#B877DB")
       (cranberry "#E95678")
       (turquoise "#25B0BC")
       (apricot "#F09483")
       (rosebud "#FAB795")
       (tacao "#FAC29A")
       (grey "#6a6a6a")
       ;; ui
       (shadow "#16161C")
       (border "#1A1C23")
       (background "#1C1E26")
       (background-alt "#232530")
       (accent "#2E303E")
       (accent-alt "#6C6F93")
       (secondary-accent "#E9436D")
       (secondary-accent-alt "#E95378")
       (tertiary-accent "#FAB38E")
       (positive "#09F7A0")
       (negative "#F43E5C")
       (warning "#27D797")
       (modified "#21BFC2")
       (light-text "#D5D8DA")
       (dark-text "#06060C")
       ;; ansi
       (blue "#3FC4DE")
       (cyan "#6BE4E6")
       (green "#3FDAA4")
       (magenta "#F075B5")
       (red "#EC6A88")
       (yellow "#FBC3A7"))
  (custom-theme-set-faces
   'horizon
   `(default ((t (:foreground ,light-text :background ,background))))
   `(cursor ((t (:background ,secondary-accent-alt :foreground ,background))))
   `(highlight ((t (:background ,accent-alt))))
   `(fringe ((t :foreground ,background-alt)))
   `(region ((t :background ,accent)))
   `(shadow ((t :foreground ,shadow)))
   `(minibuffer-prompt ((t :foreground ,turquoise)))
   `(trailing-whitespace ((t :foreground ,turquoise :background ,turquoise)))     
   `(match ((t :background ,accent :foreground ,background)))
   ;; font-lock faces
   `(font-lock-builtin-face ((t :foreground ,lavender)))
   `(font-lock-comment-face ((t :foreground ,grey)))
   `(font-lock-constant-face ((t :foreground ,apricot)))
   `(font-lock-doc-face ((t :foreground ,grey)))
   `(font-lock-function-name-face ((t :foreground ,turquoise)))
   `(font-lock-keyword-face ((t :foreground ,lavender)))
   `(font-lock-preprocessor-face ((t :foreground ,lavender)))
   `(font-lock-string-face ((t :foreground ,rosebud)))
   `(font-lock-type-face ((t :foreground ,apricot)))
   `(font-lock-variable-name-face ((t :foreground ,cranberry)))
   `(font-lock-warning-face ((t :foreground ,warning)))
   ;; diff
   `(diff-removed ((t :foreground ,negative)))
   `(diff-added ((t :foreground ,positive)))
   ;; isearch
   `(isearch ((t :foreground ,light-text :background ,accent)))
   `(isearch-lazy-highlight-face ((t :background ,red)))
   ;; mode-line
   `(mode-line ((t :background ,background-alt :foreground ,light-text)))
   `(mode-line-buffer-id ((t :foreground ,apricot)))
   `(mode-line-inactive ((t :foreground ,background)))
   ))

