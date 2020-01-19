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

(defun join-colour (r g b)
  "Build a color from R G B.
Inverse of `color-values'."
  (format "#%02x%02x%02x"
          (ash r -8)
          (ash g -8)
          (ash b -8)))

(defun blend-colours (c1 c2 &optional alpha)
  "Blend the two colors C1 and C2 with ALPHA.
C1 and C2 are in the format of `color-values'.
ALPHA is a number between 0.0 and 1.0 which corresponds to the
influence of C1 on the result."
  (let ((alpha (or alpha 0.5))
        (c1 (color-values c1))
        (c2 (color-values c2)))
    (apply #'join-colour
           (cl-mapcar
            (lambda (x y)
              (round (+ (* x alpha) (* y (- 1 alpha)))))
            c1 c2))))

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
       (foreground (blend-colours light-text background 0.85))
       ;; ansi
       (blue "#3FC4DE")
       (cyan "#6BE4E6")
       (green "#3FDAA4")
       (magenta "#F075B5")
       (red "#EC6A88")
       (yellow "#FBC3A7"))
  (custom-theme-set-faces
   'horizon
   `(default ((t (:foreground ,foreground :background ,background))))
   `(cursor ((t (:background ,secondary-accent-alt :foreground ,background))))
   `(highlight ((t (:background ,accent-alt))))
   `(fringe ((t :foreground ,background-alt)))
   `(region ((t :background ,accent)))
   `(shadow ((t :foreground ,shadow)))
   `(minibuffer-prompt ((t :foreground ,turquoise)))
   `(trailing-whitespace ((t :foreground ,turquoise :background ,turquoise)))     
   `(match ((t :foreground ,foreground :background ,apricot)))
   `(lazy-highlight ((t :foreground ,light-text :background ,(blend-colours turquoise background 0.35))))
   `(warning ((t :foreground ,warning)))
   `(error ((t :foreground ,cranberry)))
   `(success ((t :foreground ,turquoise)))
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
   `(mode-line-inactive ((t :foreground ,accent)))
   ;; markdown
   `(markdown-header-face-1 ((t :foreground ,secondary-accent-alt)))
   `(markdown-header-delimiter-face ((t :foreground ,secondary-accent-alt)))
   `(markdown-markup-face ((t :foreground ,lavender)))
   `(markdown-link-face ((t :foreground ,secondary-accent-alt :underline t)))
   `(markdown-url-face ((t :foreground ,turquoise)))
   `(markdown-bold-face ((t :foreground ,lavender :bold t)))
   `(markdown-italic-face ((t :foreground ,turquoise :italic t)))
   ))

