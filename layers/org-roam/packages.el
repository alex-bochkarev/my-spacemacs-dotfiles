;;; packages.el --- org-roam layer packages file for Spacemacs.
;;
;; Copyright (c) 2012-2018 Sylvain Benner & Contributors
;;
;; Author: Alexey Bochkarev <bochkarev@aldan3>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

;;; Commentary:

;; See the Spacemacs documentation and FAQs for instructions on how to implement
;; a new layer:
;;
;;   SPC h SPC layers RET
;;
;;
;; Briefly, each package to be installed or configured by this layer should be
;; added to `org-roam-packages'. Then, for each package PACKAGE:
;;
;; - If PACKAGE is not referenced by any other Spacemacs layer, define a
;;   function `org-roam/init-PACKAGE' to load and initialize the package.

;; - Otherwise, PACKAGE is already referenced by another Spacemacs layer, so
;;   define the functions `org-roam/pre-init-PACKAGE' and/or
;;   `org-roam/post-init-PACKAGE' to customize the package as it is loaded.

;;; Code:

(defconst org-roam-packages
  '((org-roam :location
              (recipe :fetcher github :repo "org-roam/org-roam" :branch "master"))
    (org-roam-bibtex)
    ))

(defun org-roam/init-org-roam ()
  (use-package org-roam
    :hook
    (after-init . org-roam-mode)
    :custom
    (org-roam-directory "~/zettelkasten/")
    :init
    (progn
      (spacemacs/declare-prefix "ar" "org-roam")
      (spacemacs/set-leader-keys
        "arl" 'org-roam
        "art" 'org-roam-today
        "arf" 'org-roam-find-file
        "arg" 'org-roam-show-graph)

      (spacemacs/declare-prefix-for-mode 'org-mode "mr" "org-roam")
      (spacemacs/set-leader-keys-for-major-mode 'org-mode
        "rl" 'org-roam
        "rt" 'org-roam-today
        "rb" 'org-roam-switch-to-buffer
        "rf" 'org-roam-find-file
        "ri" 'org-roam-insert
        "rg" 'org-roam-show-graph)
      (setq org-roam-link-title-format "🕮:%s")
      (require 'org-roam-protocol)
      ;; end of config
      )
    ))

(defun org-roam/init-org-roam-bibtex ()
  ;; org-roam-bibtex setup
  (use-package org-roam-bibtex
    :load-path "~/.spacemacs.d/distribs/org-roam-bibtex" ;Modify with your own path
    :hook (org-roam-mode . org-roam-bibtex-mode)
    :bind (:map org-mode-map
                (("H-b" . orb-note-actions)))
    :config
    (setq orb-preformat-keywords
          '("citekey" "title" "author" "keywords"))
    (setq orb-templates
          '(("r" "ref" plain (function org-roam-capture--get-point) ""
             :file-name "${citekey}"
             :head "#+TITLE: ${citekey}: ${title}
#+ROAM_KEY: ${citekey}
#+PROPERTY: authors ${author}
#+TAGS: ${keywords}\n\n"
             :unnarrowed t)))
    )
  )

;;   '()
;;   "The list of Lisp packages required by the org-roam layer.

;; Each entry is either:

;; 1. A symbol, which is interpreted as a package to be installed, or

;; 2. A list of the form (PACKAGE KEYS...), where PACKAGE is the
;;     name of the package to be installed or loaded, and KEYS are
;;     any number of keyword-value-pairs.

;;     The following keys are accepted:

;;     - :excluded (t or nil): Prevent the package from being loaded
;;       if value is non-nil

;;     - :location: Specify a custom installation location.
;;       The following values are legal:

;;       - The symbol `elpa' (default) means PACKAGE will be
;;         installed using the Emacs package manager.

;;       - The symbol `local' directs Spacemacs to load the file at
;;         `./local/PACKAGE/PACKAGE.el'

;;       - A list beginning with the symbol `recipe' is a melpa
;;         recipe.  See: https://github.com/milkypostman/melpa#recipe-format")


;;; packages.el ends here
