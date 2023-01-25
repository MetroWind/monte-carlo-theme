;;; monte-carlo-theme.el --- A theme with random colors

;; Copyright (C) 2021 MetroWind.

;; This program is free software. It comes without any warranty, to
;; the extent permitted by applicable law. You can redistribute it
;; and/or modify it under the terms of the Do What the Fuck You Want
;; to Public License, Version 2, as published by Sam Hocevar. See
;; http://www.wtfpl.net/ for more details.

;; Author: MetroWind <chris.corsair@gmail.com>
;; URL: https://github.com/MetroWind/notink-theme
;; Keywords: lisp
;; Version: 1.0
;; Package-Requires: ((emacs "24"))

;;; Commentary:
;;
;; Monte Carlo theme is a custom theme for Emacs. It uses a random set
;; of colors each time.

;;; Code:

(deftheme monte-carlo "A theme with random colors")

(defun monte-carlo-theme-create-color-sheet ()
  (interactive)
  (defun create-color-sheet (color-set)
    (set-buffer (generate-new-buffer "*Color Sheet*"))
    (let ((color-hex (hlc2srgb-hex (bg color-set))))
      (insert "bg                  " color-hex "\n")
      (overlay-put (make-overlay 12 20) 'face
                   (cons 'background-color color-hex)))
    (let ((color-hex (hlc2srgb-hex (fg color-set))))
      (insert "fg                  " color-hex "\n")
      (overlay-put (make-overlay 40 48) 'face
                   (cons 'background-color color-hex)))
    (let ((color-hex (hlc2srgb-hex (color1 color-set))))
      (insert "color1              " color-hex "\n")
      (overlay-put (make-overlay 68 76) 'face
                   (cons 'background-color color-hex)))
    (let ((color-hex (hlc2srgb-hex (color2 color-set))))
      (insert "color2              " color-hex "\n")
      (overlay-put (make-overlay 96 104) 'face
                   (cons 'background-color color-hex)))
    (let ((color-hex (hlc2srgb-hex (color3 color-set))))
      (insert "color3              " color-hex "\n")
      (overlay-put (make-overlay 124 132) 'face
                   (cons 'background-color color-hex)))
    (let ((color-hex (hlc2srgb-hex (color4 color-set))))
      (insert "color4              " color-hex "\n")
      (overlay-put (make-overlay 152 160) 'face
                   (cons 'background-color color-hex)))
    (let ((color-hex (hlc2srgb-hex (color5 color-set))))
      (insert "color5              " color-hex "\n")
      (overlay-put (make-overlay 180 188) 'face
                   (cons 'background-color color-hex)))
    (read-only-mode))
  (create-color-sheet monte-carlo-theme-color-set))

;; Colors
(defun monte-carlo-theme-apply ()
  (interactive)
  (defun lab2xyz (lab-color)
    "Convert `lab-color` (vector of floats) to XYZ color. l ∈ [0,
100], a,b ∈ [-128, 127]. Reference:
http://www.brucelindbloom.com/index.html?Math.html"
    (let* ((epsilon 0.008856)
           (kappa 903.3)
           (l (elt lab-color 0))
           (a (elt lab-color 1))
           (b (elt lab-color 2))
           (fy (/ (+ l 16.0) 116.0))
           (fx (+ (/ a 500.0) fy))
           (fz (- fy (/ b 200.0)))
           (fx3 (expt fx 3.0))
           (fz3 (expt fz 3.0))
           (xr (if (> fx3 epsilon) fx3 (/ (- (* 116.0 fx) 16.0) kappa)))
           (yr (if (> l (* kappa epsilon)) (expt fy 3.0) (/ l kappa)))
           (zr (if (> fz3 epsilon) fz3 (/ (- (* 116.0 fz) 16.0) kappa))))
      (vector (* xr 0.95047) yr (* zr 1.08883))))

  (defun hlc2lab (hlc-color)
    "Convert CIEHLC color `hlc-color` to CIELAB. h ∈ [0, 2π), l ∈ [0,
100], c ∈ [0, 128]. Reference:
https://en.wikipedia.org/wiki/CIELAB_color_space#CIEHLC_cylindrical_model"
    (let ((h (elt hlc-color 0))
          (l (elt hlc-color 1))
          (c (elt hlc-color 2)))
      (vector l (* c (cos h)) (* c (sin h)))))

  (defun xyz2srgb (xyz)
    "Convert CIEXYZ color to sRGB. Result is from 0 to 1. Reference:
https://darksair.org/wiki/color-science/main.html"
    (defun not-gamma-map (x)
      (if (> x 0.0031308)
          (- (* 1.055 (expt x (/ 1 2.4))) 0.055)
        (* 12.92 x)))
    (let* ((x (elt xyz 0))
           (y (elt xyz 1))
           (z (elt xyz 2))
           (r (+ (* 3.2406 x) (* -1.5372 y) (* -0.4986 z)))
           (g (+ (* -0.9689 x) (* 1.8758 y) (* 0.0415 z)))
           (b (+ (* 0.0557 x) (* -0.2040 y) (* 1.0570 z))))
      (vector (not-gamma-map r) (not-gamma-map g) (not-gamma-map b))))

  (defun hlc2srgb (hlc)
    (xyz2srgb (lab2xyz (hlc2lab hlc))))

  (defun srgb2hex (srgb)
    (defun clamp (x min max)
      (cond ((< x min) min)
            ((> x max) max)
            (t x)))
    (let ((r (clamp (* (elt srgb 0) 255) 0 255))
          (g (clamp (* (elt srgb 1) 255) 0 255))
          (b (clamp (* (elt srgb 2) 255) 0 255)))
      (format "#%02x%02x%02x" r g b)))

  (defun hlc2srgb-hex (hlc)
    (srgb2hex (hlc2srgb hlc)))

  ;; Lightness ∈ [0, 1], saturation ∈ [0, 1]
  (defun random-color (lightness saturation)
    (hlc2srgb-hex (vector (/ (random 360) (* 2.0 3.14159)) lightness saturation)))

  (defun random-float (min max)
    (let ((min000 (floor (* min 1000.0)))
          (max000 (floor (* max 1000.0))))
      (* (+ (random (- max000 min000)) min000) 0.001)))

  (cl-defun make-color-gen-options
      (&key bg-fg-contrast bg-fg-saturation
            color-contrast color-saturation)
    (let ((result (make-hash-table :size 4)))
      (puthash 'bg-fg-contrast bg-fg-contrast result)
      (puthash 'bg-fg-saturation bg-fg-saturation result)
      (puthash 'color-contrast color-contrast result)
      (puthash 'color-saturation color-saturation result)
      result))

  (defun make-color (h l c)
    (vector h l c))

  (defun h (color) (elt color 0))
  (defun l (color) (elt color 1))
  (defun c (color) (elt color 2))

  (defun make-color-set (bg fg color1 color2 color3 color4 color5)
    (vector bg fg color1 color2 color3 color4 color5))

  (defun bg (color-set)
    (elt color-set 0))

  (defun fg (color-set)
    (elt color-set 1))

  (defun color1 (color-set)
    (elt color-set 2))
  (defun color2 (color-set)
    (elt color-set 3))
  (defun color3 (color-set)
    (elt color-set 4))
  (defun color4 (color-set)
    (elt color-set 5))
  (defun color5 (color-set)
    (elt color-set 6))

  (defun color-lighter (color delta-l)
    (make-color (h color) (+ (l color) delta-l) (c color)))

  (defun color-darker (color delta-l)
    (make-color (h color) (- (l color) delta-l) (c color)))

  (defun ring-distance (ring-min ring-max x1 x2)
    (if (>= x2 x1)
        (min (- x2 x1) (+ (- ring-max x2) (- x1 ring-min)))
      (min (- x1 x2) (+ (- ring-max x1) (- x2 ring-min)))))

  (defun ring-wrap (ring-min ring-max x)
    (let ((winding-number (ffloor (/ (- x ring-min) (- ring-max ring-min)))))
      (- x (+ (* winding-number (- ring-max ring-min)) ring-min))))

  (defun ring+ (ring-min ring-max x addition)
    (let ((xp (+ x addition)))
      (if (> xp ring-max)
          (ring-wrap xp)
        xp)))

  (defun divide-ring-roughly (ring-min ring-max divide-count perturb)
    (defun divide-ring-inner (ring-min ring-max perturb exact-size exact-cut
                                       cut-left)
      (if (= cut-left 0)
          nil
        (cons (ring-wrap ring-min ring-max
                         (random-float (- exact-cut perturb)
                                       (+ perturb exact-cut)))
              (divide-ring-inner ring-min ring-max perturb exact-size
                                 (+ exact-cut exact-size) (- cut-left 1)))))
    (let ((first-cut (random-float ring-min ring-max))
          (exact-size (/ (- ring-max ring-min) (float divide-count))))
      (cons first-cut
            (divide-ring-inner ring-min ring-max perturb exact-size
                               (+ first-cut exact-size) (- divide-count 1)))))

  (defun gen-color-set (options)
    (defun gen-accent-colors (options)
      (let* ((2pi (* 2.0 3.14159265))
             (contrast (gethash 'color-contrast options))
             (sat (gethash 'color-saturation options))
             (hues (divide-ring-roughly 0 2pi 5 0.15)))
        (mapcar (lambda (hue) (vector hue (+ 50.0 (* 50.0 contrast)) (* sat 128.0)))
                hues)))

    (let* ((2pi (* 2.0 3.14159265))
           (contrast (gethash 'bg-fg-contrast options))
           (sat (gethash 'bg-fg-saturation options))
           (bg (make-color (random-float 0 2pi)
                           (- 50.0 (* 50.0 contrast))
                           (* sat 128.0)))
           (fg (make-color (random-float 0 2pi)
                           (+ 50.0 (* 50.0 contrast))
                           (* sat 128.0)))
           (colors (gen-accent-colors options)))
      (make-color-set bg fg (nth 0 colors) (nth 1 colors) (nth 2 colors) (nth 3 colors)
                      (nth 4 colors))))

  (let*
      ((color-set (gen-color-set
                   (make-color-gen-options :bg-fg-contrast 0.7 :bg-fg-saturation 0.04
                                           :color-contrast 0.5 :color-saturation 0.3)))
       (color-fg (hlc2srgb-hex (fg color-set)))
       (color-bg (hlc2srgb-hex (bg color-set)))
       (color-1 (hlc2srgb-hex (color1 color-set)))
       (color-2 (hlc2srgb-hex (color2 color-set)))
       (color-3 (hlc2srgb-hex (color3 color-set)))
       (color-4 (hlc2srgb-hex (color4 color-set)))
       (color-5 (hlc2srgb-hex (color5 color-set)))
       (color-void (hlc2srgb-hex (color-darker (bg color-set) 15)))
       (color-dark (hlc2srgb-hex (color-lighter (bg color-set) 15)))
       (color-middle (hlc2srgb-hex (color-lighter (bg color-set) 30)))
       (color-light (hlc2srgb-hex (color-darker (fg color-set) 15)))
       (color-bright (hlc2srgb-hex (color-lighter (fg color-set) 15)))
       )
    (setq monte-carlo-theme-color-set color-set)
    (custom-theme-set-faces
     'monte-carlo
     `(default ((t (:background ,color-bg
                                :foreground ,color-fg))))
     `(cursor ((t (:background ,color-1
                               :foreground ,color-fg))))
     `(region ((t (:background ,color-2
                               :foreground ,color-bg))))
     `(mode-line ((t (:background ,color-void
                                  :foreground ,color-fg
                                  :box nil))))
     `(mode-line-buffer-id ((t (:foreground ,color-fg))))
     `(mode-line-inactive ((t (:background ,color-dark
                                           :foreground ,color-fg))))
     `(fringe ((t (:background ,color-bg))))
     `(minibuffer-prompt ((t (:inherit italic :foreground ,color-light))))
     `(font-lock-builtin-face ((t (:foreground ,color-light))))
     `(font-lock-comment-face ((t (:inherit italic :foreground ,color-middle))))
     `(font-lock-constant-face ((t (:inherit italic :foreground ,color-1))))
     `(font-lock-function-name-face ((t (:foreground ,color-2))))
     `(font-lock-keyword-face ((t (:foreground ,color-3 :inherit italic))))
     `(font-lock-string-face ((t (:foreground ,color-4))))
     `(font-lock-type-face ((t (:foreground ,color-5))))
     `(font-lock-variable-name-face ((t (:foreground ,color-1))))
     `(font-lock-warning-face ((t (:foreground ,color-1))))

     `(isearch ((t (:background ,color-light
                                :foreground ,color-bg))))
     `(lazy-highlight ((t (:background ,color-void))))
     `(link ((t (:foreground ,color-1 :underline t))))
     `(link-visited ((t (:foreground ,color-middle :underline t))))
     `(button ((t (:foreground ,color-2 :underline t :background nil))))
     `(header-line ((t (:background ,color-void
                                    :foreground ,color-fg))))
     `(shadow ((t (:foreground ,color-middle))))
     `(show-paren-match ((t (:background ,color-2 :foreground ,color-bg))))
     `(show-paren-mismatch ((t (:background ,color-3
                                            :foreground ,color-bg))))
     `(highlight ((t (:inverse-video nil :background ,color-void))))
     `(hl-line ((t (:inverse-video nil :background ,color-void))))
     `(widget-field ((t (:background ,color-middle))))

     ;; Faces for specific prog modes
     `(sh-heredoc ((t (:foreground nil :inherit font-lock-string-face))))

     ;; Dired
     `(dired-directory ((t (:foreground ,color-1))))
     `(dired-symlink ((t (:foreground ,color-3))))
     `(dired-perm-write ((t (:foreground ,color-4))))

     ;; Diff
     `(diff-added ((t (:foreground ,color-1))))
     `(diff-removed ((t (:foreground ,color-4))))
     ;; `(diff-context ((t (:background nil))))
     `(diff-file-header ((t (:bold t :background ,color-dark :weight bold))))
     `(diff-header ((t (:background ,color-void :foreground ,color-fg))))

     ;; Whitespace
     `(whitespace-trailing ((t (:background ,color-dark))))
     `(whitespace-line ((t (:background ,color-dark :foreground unspecified))))

     ;; ERC
     `(erc-notice-face ((t (:foreground ,color-1
                                        :weight unspecified))))
     `(erc-header-line ((t (:foreground ,color-bg :background ,color-void))))
     `(erc-timestamp-face ((t (:foreground ,color-middle
                                           :weight unspecified))))
     `(erc-current-nick-face ((t (:foreground ,color-2
                                              :weight unspecified))))
     `(erc-input-face ((t (:foreground ,color-3))))
     `(erc-prompt-face ((t (:foreground ,color-middle
                                        :background nil
                                        :inherit italic
                                        :weight unspecified))))
     `(erc-my-nick-face ((t (:foreground ,color-2))))
     `(erc-pal-face ((t (:foreground ,color-4))))

     ;; Rainbow delimiters
     `(rainbow-delimiters-depth-1-face ((t (:foreground ,color-fg))))
     `(rainbow-delimiters-depth-2-face ((t (:foreground ,color-1))))
     `(rainbow-delimiters-depth-3-face ((t (:foreground ,color-2))))
     `(rainbow-delimiters-depth-4-face ((t (:foreground ,color-3))))
     `(rainbow-delimiters-depth-5-face ((t (:foreground ,color-4))))
     `(rainbow-delimiters-depth-6-face ((t (:foreground ,color-5))))
     `(rainbow-delimiters-depth-7-face ((t (:foreground ,color-middle))))
     `(rainbow-delimiters-unmatched-face ((t (:foreground ,color-3))))

     ;; Magit
     `(magit-branch-local ((t (:foreground ,color-1 :background nil))))
     `(magit-branch-remote ((t (:foreground ,color-2 :background nil))))
     `(magit-tag ((t (:foreground ,color-1 :background ,color-bg))))
     `(magit-hash ((t (:foreground ,color-middle))))
     `(magit-section-title ((t (:foreground ,color-2 :background ,color-bg))))
     `(magit-section-heading ((t (:background ,color-bg :foreground ,color-fg))))
     `(magit-section-highlight ((t (:background ,color-bg))))
     `(magit-item-highlight ((t (:foreground ,color-fg :background ,color-dark))))
     `(magit-log-author ((t (:foreground ,color-3))))
     `(magit-diff-added ((t (:inherit diff-added))))
     `(magit-diff-added-highlight ((t (:inherit magit-diff-added))))
     `(magit-diff-removed ((t (:inherit diff-removed))))
     `(magit-diff-removed-highlight ((t (:inherit magit-diff-removed))))
     `(magit-diff-context ((t (:inherit diff-context))))
     `(magit-diff-context-highlight ((t (:inherit magit-diff-context))))
     `(magit-popup-argument ((t (:inherit font-lock-function-name-face))))
     `(magit-popup-disabled-argument ((t (:inherit font-lock-comment-face))))

     ;; Git-gutter-fringe
     `(git-gutter-fr:modified ((t (:foreground ,color-5))))
     `(git-gutter-fr:added ((t (:foreground ,color-2))))
     `(git-gutter-fr:deleted ((t (:foreground ,color-3))))

     ;; Company
     `(company-preview ((t (:foreground ,color-fg :background ,color-3))))
     `(company-preview-common ((t (:foreground ,color-fg :background ,color-4))))
     `(company-tooltip ((t (:foreground ,color-fg :background ,color-void))))
     `(company-tooltip-common ((t (:foreground ,color-4))))
     `(company-tooltip-selection ((t (:background ,color-dark))))
     `(company-tooltip-common-selection ((t (:foreground ,color-4))))
     `(company-tooltip-annotation ((t (:foreground ,color-1))))
     `(company-scrollbar-bg ((t (:background ,color-void))))
     `(company-scrollbar-fg ((t (:background ,color-dark))))

     ;; Powerline
     `(powerline-active2 ((t (:foreground ,color-fg :background ,color-bg))))
     `(powerline-active1 ((t (:foreground ,color-bg :background ,color-2))))
     `(powerline-inactive2 ((t (:foreground ,color-bg :background ,color-dark))))
     `(powerline-inactive1 ((t (:foreground ,color-fg :background ,color-void))))

     ;; Smart mode line
     `(sml/global  ((t (:foreground ,color-fg))))
     `(sml/charging ((t (:foreground ,color-2))))
     `(sml/discharging ((t (:foreground ,color-3))))
     `(sml/read-only ((t (:foreground ,color-2))))
     `(sml/filename ((t (:foreground ,color-1 :weight normal))))
     `(sml/prefix ((t (:foreground ,color-5 :weight normal :inherit italic))))
     `(sml/modes ((t (:foreground ,color-fg :weight normal))))
     `(sml/modified ((t (:foreground ,color-4))))
     `(sml/outside-modified ((t (:foreground ,color-bg :background ,color-4))))
     `(sml/position-percentage ((t (:foreground ,color-5 :slant normal))))

     ;; Helm
     `(helm-candidate-number ((t (:foreground ,color-fg :background nil))))
     `(helm-source-header ((t (:foreground ,color-bg :background ,color-1
                                           :weight normal :inherit italic))))
     `(helm-selection ((t (:inherit region :distant-foreground nil :background nil))))
     `(helm-prefarg ((t (:foreground ,color-4))))
     `(helm-ff-directory ((t (:foreground ,color-1))))
     `(helm-ff-executable ((t (:foreground ,color-2))))
     `(helm-ff-invalid-symlink ((t (:foreground ,color-bg
                                                :background ,color-3))))
     `(helm-ff-symlink ((t (:foreground ,color-5))))
     `(helm-ff-prefix ((t (:background ,color-3))))
     `(helm-ff-dotted-directory ((t (:background nil :foreground ,color-dark))))
     `(helm-M-x-key ((t (:foreground ,color-2))))
     `(helm-buffer-file ((t (:foreground ,color-fg))))
     `(helm-buffer-archive ((t (:inherit helm-buffer-file))))
     `(helm-buffer-directory ((t (:foreground ,color-1 :background nil))))
     `(helm-buffer-not-saved ((t (:foreground ,color-4))))
     `(helm-buffer-modified ((t (:foreground ,color-4))))
     `(helm-buffer-process ((t (:foreground ,color-2))))
     `(helm-buffer-size ((t (:foreground ,color-dark))))
     `(helm-ff-file ((t (:inherit default))))

     ;; TeX
     `(font-latex-sedate-face ((t (:foreground ,color-1))))
     `(font-latex-math-face ((t (:foreground ,color-4))))
     `(font-latex-script-char-face ((t (:inherit font-latex-math-face))))

     ;; adoc-mode
     `(markup-meta-hide-face ((t (:height 1.0 :foreground ,color-fg))))
     `(markup-meta-face ((t (:height 1.0 :foreground ,color-fg :family nil))))
     `(markup-reference-face ((t (:underline nil :foreground ,color-1))))
     `(markup-gen-face ((t (:foreground ,color-1))))
     `(markup-passthrough-face ((t (:inherit markup-gen-face))))
     `(markup-replacement-face ((t (:family nil :foreground ,color-2))))
     `(markup-list-face ((t (:weight bold))))
     `(markup-secondary-text-face ((t (:height 1.0 :foreground ,color-2))))
     `(markup-verbatim-face ((t (:foreground ,color-light))))
     `(markup-typewriter-face ((t (:inherit nil))))
     `(markup-title-0-face ((t (:height 1.2 :inherit markup-gen-face))))
     `(markup-title-1-face ((t (:height 1.0 :inherit markup-gen-face))))
     `(markup-title-2-face ((t (:height 1.0 :inherit markup-gen-face))))
     `(markup-title-3-face ((t (:height 1.0 :inherit markup-gen-face))))
     `(markup-title-4-face ((t (:height 1.0 :inherit markup-gen-face))))
     `(markup-title-5-face ((t (:height 1.0 :inherit markup-gen-face))))

     ;; Outline
     `(outline-1 ((t (:foreground ,color-5))))
     `(outline-2 ((t (:foreground ,color-1))))
     `(outline-3 ((t (:foreground ,color-2))))
     `(outline-4 ((t (:foreground ,color-3))))
     `(outline-5 ((t (:foreground ,color-4))))
     `(outline-6 ((t (:foreground ,color-fg))))
     `(outline-7 ((t (:foreground ,color-fg :inherit italic))))
     `(outline-8 ((t (:foreground ,color-light))))

     ;; Org-mode
     `(org-hide ((t (:foreground ,color-bg))))
     `(org-table ((t (:foreground ,color-fg))))
     `(org-date ((t (:foreground ,color-2))))
     `(org-done ((t (:weight normal :foreground ,color-light))))
     `(org-todo ((t (:weight normal :foreground ,color-3))))
     `(org-latex-and-related ((t (:foreground ,color-middle :italic t))))
     `(org-checkbox ((t (:weight normal :foreground ,color-light))))
     `(org-verbatim ((t (:foreground ,color-middle))))
     `(org-mode-line-clock ((t (:background nil))))
     `(org-document-title ((t (:weight normal :foreground nil))))

     ;; org-tree-slide
     `(org-tree-slide-header-overlay-face
       ((t (:inherit font-lock-comment-face :foreground nil :background nil))))

     ;; Message
     `(message-header-name ((t (:foreground ,color-light))))
     `(message-header-other ((t (:foreground ,color-fg))))
     `(message-header-cc ((t (:inherit message-header-other))))
     `(message-header-newsgroups ((t (:inherit message-header-other))))
     `(message-header-xheader ((t (:inherit message-header-other))))
     `(message-header-subject ((t (:foreground ,color-2))))
     `(message-header-to ((t (:foreground ,color-1))))
     `(message-cited-text ((t (:foreground ,color-3))))
     `(message-mml ((t (:foreground ,color-middle))))

     ;; Notmuch
     `(notmuch-search-unread-face ((t (:foreground ,color-1))))
     `(notmuch-tag-face ((t (:foreground ,color-2))))
     `(notmuch-tree-match-author-face ((t (:foreground ,color-1))))
     `(notmuch-tree-no-match-face ((t (:foreground ,color-light))))
     `(notmuch-tree-match-tag-face ((t (:inherit notmuch-tree-match-author-face))))
     `(notmuch-tag-unread-face ((t (:foreground ,color-4))))
     `(notmuch-message-summary-face ((t (:foreground ,color-middle))))

     ;; Compilation
     `(compilation-error ((t (:foreground ,color-4))))
     `(compilation-info ((t (:foreground ,color-2))))
     `(compilation-warning ((t (:foreground ,color-3))))

     ;; Highlight-indent-guides
     `(highlight-indent-guides-odd-face ((t (:background ,color-void))))
     `(highlight-indent-guides-even-face ((t (:background nil))))

     ;; Telega
     `(telega-msg-heading ((t (:background nil :foreground ,color-2 :inherit nil))))
     `(telega-msg-inline-reply ((t (:foreground ,color-light :inherit nil))))
     `(telega-entity-type-texturl ((t (:inherit nil :foreground ,color-1))))
     )))

(monte-carlo-theme-apply)
(provide-theme 'monte-carlo)

;; Local Variables:
;; no-byte-compile: t
;; End:

;;; monte-carlo-theme.el ends here
