;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!


;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets.
(setq user-full-name "Alan Severini"
      user-mail-address "severini.alan@gmail.com")

;; Doom exposes five (optional) variables for controlling fonts in Doom. Here
;; are the three important ones:
;;
;; + `doom-font'
;; + `doom-variable-pitch-font'
;; + `doom-big-font' -- used for `doom-big-font-mode'; use this for
;;   presentations or streaming.
;;
;; They all accept either a font-spec, font string ("Input Mono-12"), or xlfd
;; font string. You generally only need these two:
;; (setq doom-font (font-spec :family "monospace" :size 12 :weight 'semi-light)
;;       doom-variable-pitch-font (font-spec :family "sans" :size 13))

;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. This is the default:
(setq doom-theme 'doom-vibrant)

(set-face-attribute 'default nil :height 105 :family "monospace" :weight 'normal :width 'normal)

;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
(setq org-directory "~/Dropbox/org")
(setq org-support-shift-select t)

;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
(setq display-line-numbers-type t)

(blink-cursor-mode 1)
(display-time-mode 1)
(display-battery-mode 1)

(setq tab-always-indent 'complete)

;; Disable overwrite-mode for ever!
(define-key global-map [(insert)] nil)

;; Here are some additional functions/macros that could help you configure Doom:
;;
;; - `load!' for loading external *.el files relative to this one
;; - `use-package!' for configuring packages
;; - `after!' for running code after a package has loaded
;; - `add-load-path!' for adding directories to the `load-path', relative to
;;   this file. Emacs searches the `load-path' when you load packages with
;;   `require' or `use-package'.
;; - `map!' for binding new keys
;;
;; To get information about any of these functions/macros, move the cursor over
;; the highlighted symbol at press 'K' (non-evil users must press 'C-c c k').
;; This will open documentation for it, including demos of how they are used.
;;
;; You can also try 'gd' (or 'C-c c d') to jump to their definition and see how
;; they are implemented.
;;
;;

(autoload 'helm-bibtex "helm-bibtex" "" t)


(setq
 org_notes "~/Dropbox/org/roam"
 zot_bib "~/Dropbox/org/library.bib"
 ;; org-directory org_notes
 deft-directory org_notes
 org-roam-directory org_notes
 org-default-notes-file (concat org_notes "/bibnotes.org")
 )

(use-package! ess
  :hook (ess-mode-hook . forbid-vertical-split)
  :init
  (require 'ess-site)
  (require 'ess-mode)
  (require 'ess-r-mode)
  (setq ess-indent-with-fancy-comments nil)
  :config
  (defun then_R_operator ()
    "R - %>% operator or 'then' pipe operator"
    (interactive)
    (just-one-space 1)
    (insert "%>%")
    (reindent-then-newline-and-indent))
  (defun then_ggplot_plus ()
    (interactive)
    (just-one-space 1)
    (insert "+")
    (reindent-then-newline-and-indent))
  (defun forbid-vertical-split ()
    "Only permit horizontal window splits."
    (setq-local split-height-threshold nil)
    (setq-local split-width-threshold 0))
  (setq ess-ask-for-ess-directory nil
	ess-local-process-name "R"
	ansi-color-for-comint-mode 'filter
	comint-scroll-to-bottom-on-input t
	comint-scroll-to-bottom-on-output t
	comint-move-point-for-output t
        ess-use-flymake nil
        ess-eval-visibly nil
	ess-default-style 'RStudio
	fill-column 72
	comment-auto-fill-only-comments t)
  :bind (:map ess-mode-map
	 ("C-<return>" . 'then_R_operator)
	 ("M-<return>" . 'then_ggplot_plus)
	 ("_" . 'ess-insert-assign)
	 ("S-<return>" . 'ess-eval-region-or-function-or-paragraph-and-step)
         :map inferior-ess-r-mode-map
	 ("C-<return>" . 'then_R_operator)
	 ("M-<return>" . 'then_ggplot_plus)
	 ("_" . 'ess-insert-assign)
         ("S-<return>" . 'ess-eval-region-or-function-or-paragraph-and-step)))

(after! org (setq org-agenda-files (append (file-expand-wildcards "~/Dropbox/org/*.org"))))

(after! ivy
  (setq ivy-use-virtual-buffers t))

;; (use-package! bibtex-completion
;;   :config
;;   (setq bibtex-completion-bibliography "~/Dropbox/org/library.bib"
;;         bibtex-completion-pdf-field "file"
;;         ;; ivy-bibtex-default-action 'ivy-bibtex-insert-citation
;;         ))

(use-package! deft
  :after org
  :bind
  ("<f6>" . deft)
  :custom
  (deft-recursive t)
  (deft-use-filter-string-for-filename t)
  (deft-default-extension "org")
<<<<<<< HEAD
  (deft-directory "~/Dropbox/org/roam/" ))

(use-package! zetteldeft
  :after deft
  :config (zetteldeft-set-classic-keybindings))
=======
  (deft-directory "~/Dropbox/org/roam/"))

;; (use-package! org-roam-bibtex
;;   :after org-roam
;;   :load-path "~/projects/org-roam-bibtex/"
;;   :hook (org-roam-mode . org-roam-bibtex-mode))

;; (setq org-ref-completion-library 'org-ref-ivy-cite)

;; (use-package! org-ref
;;   :after org
;;   :config
;;   (setq org-ref-notes-directory "~/Dropbox/org"
;;         org-ref-default-bibliography '("~/Dropbox/org/library.bib")
;;         org-ref-pdf-directory "~/Dropbox/Papers/"))

(use-package! org-ref
    :config
    (setq
         org-ref-completion-library 'org-ref-ivy-cite
         org-ref-get-pdf-filename-function 'org-ref-get-pdf-filename-helm-bibtex
         org-ref-default-bibliography (list "/home/alancho/Dropbox/org/library.bib")
         org-ref-bibliography-notes "/home/alancho/Dropbox/org/bibnotes.org"
         org-ref-note-title-format "* TODO %y - %t\n :PROPERTIES:\n  :Custom_ID: %k\n  :NOTER_DOCUMENT: %F\n :ROAM_KEY: cite:%k\n  :AUTHOR: %9a\n  :JOURNAL: %j\n  :YEAR: %y\n  :VOLUME: %v\n  :PAGES: %p\n  :DOI: %D\n  :URL: %U\n :END:\n\n"
         org-ref-notes-directory "/home/alancho/Dropbox/org/"
         org-ref-notes-function 'orb-edit-notes
    ))

;; (use-package! org-journal
;;       ;; :bind
;;       ;; ("C-c n j" . org-journal-new-entry)
;;       :custom
;;       (org-journal-dir "~/Dropbox/org/roam/")
;;       (org-journal-date-prefix "#+TITLE: ")
;;       (org-journal-file-format "%Y-%m-%d.org")
;;       (org-journal-date-format "%A, %d %B %Y"))
;; (setq org-journal-enable-agenda-integration t)

;; org-journal the DOOM way
(use-package! org-journal
  :bind
  ("<f7>" . org-journal-new-entry)
  :init
  (setq org-journal-dir "~/Dropbox/org/roam/"
        org-journal-date-prefix "#+TITLE: "
        org-journal-file-format "%Y-%m-%d.org"
        org-journal-date-format "%A, %d %B %Y")
  :config
  (setq org-journal-find-file #'find-file-other-window )
  )

(setq org-journal-enable-agenda-integration t)

;; (after! org-roam
;;       (setq org-roam-capture-ref-templates
;;             '(("r" "ref" plain (function org-roam-capture--get-point)
;;                "%?"
;;                :file-name "websites/${slug}"
;;                :head "#+TITLE: ${title}
;;     #+ROAM_KEY: ${ref}
;;     - source :: ${ref}"
;;                :unnarrowed t))))

(setq
 bibtex-completion-notes-path org_notes
 bibtex-completion-bibliography zot_bib
 bibtex-completion-pdf-field "file"
 bibtex-completion-notes-template-multiple-files
 (concat
  "#+TITLE: ${title}\n"
  "#+ROAM_KEY: cite:${=key=}\n"
  "* TODO Notes\n"
  ":PROPERTIES:\n"
  ":Custom_ID: ${=key=}\n"
  ":NOTER_DOCUMENT: %(orb-process-file-field \"${=key=}\")\n"
  ":AUTHOR: ${author-abbrev}\n"
  ":JOURNAL: ${journaltitle}\n"
  ":DATE: ${date}\n"
  ":YEAR: ${year}\n"
  ":DOI: ${doi}\n"
  ":URL: ${url}\n"
  ":END:\n\n"
  )
 )

;; (use-package! org-ref
;;   :config
;;   (setq
;;    org-ref-completion-library 'org-ref-ivy-cite
;;    org-ref-get-pdf-filename-function 'org-ref-get-pdf-filename-helm-bibtex
;;    org-ref-default-bibliography (list zot_bib)
;;    org-ref-bibliography-notes (concat org_notes "/bibnotes.org")
;;    org-ref-note-title-format "* TODO %y - %t\n :PROPERTIES:\n  :Custom_ID: %k\n  :NOTER_DOCUMENT: %F\n :ROAM_KEY: cite:%k\n  :AUTHOR: %9a\n  :JOURNAL: %j\n  :YEAR: %y\n  :VOLUME: %v\n  :PAGES: %p\n  :DOI: %D\n  :URL: %U\n :END:\n\n"
;;    org-ref-notes-directory org_notes
;;    org-ref-notes-function 'orb-edit-notes
;;    ))

;; (use-package! org-roam-bibtex
;;   :after (org-roam)
;;   :hook (org-roam-mode . org-roam-bibtex-mode)
;;   :config
;;   (setq orb-preformat-keywords
;;         '("=key=" "title" "url" "file" "author-or-editor" "keywords"))
;;   (setq orb-templates
;;         '(("r" "ref" plain (function org-roam-capture--get-point)
;;            ""
;;            :file-name "${slug}"
;;            :head "#+TITLE: ${=key=}: ${title}\n#+ROAM_KEY: ${ref}
;; - tags ::
;; - keywords :: ${keywords}
;; \n* ${title}\n  :PROPERTIES:\n  :Custom_ID: ${=key=}\n  :URL: ${url}\n  :AUTHOR: ${author-or-editor}\n  :NOTER_DOCUMENT: %(orb-process-file-field \"${=key=}\")\n  :NOTER_PAGE: \n  :END:\n\n"
;;            :unnarrowed t))))
>>>>>>> a9c508cb07a3dd2deb34921b312a4cf1aed7b3ba
