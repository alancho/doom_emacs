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
(setq org-directory "~/Dropbox/org/")

;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
(setq display-line-numbers-type t)

(blink-cursor-mode 1)
(display-time-mode 1)
(display-battery-mode 1)

(setq tab-always-indent 'complete)

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

(use-package! ess
  :hook (ess-mode-hook . forbid-vertical-split)
  :init
  (require 'ess-site)
  (require 'ess-mode)
  (require 'ess-r-mode)
  (setq ess-indent-with-fancy-comments nil)
  :config
  (setq ess-ask-for-ess-directory nil
	ess-local-process-name "R"
	ansi-color-for-comint-mode 'filter
	comint-scroll-to-bottom-on-input t
	comint-scroll-to-bottom-on-output t
	comint-move-point-for-output t
	ess-eval-visibly-p t
        ess-use-flymake nil
        ;; ess-eval-visibly 'nowait
	ess-default-style 'RStudio
	fill-column 72
	comment-auto-fill-only-comments t)
  ;; (auto-fill-mode t)
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

(defun org-journal-file-header-func (time)
  "Custom function to create journal header."
  (concat
    (pcase org-journal-file-type
      (`daily "#+TITLE: Daily Journal\n#+STARTUP: showeverything")
      (`weekly "#+TITLE: Weekly Journal\n#+STARTUP: folded")
      (`monthly "#+TITLE: Monthly Journal\n#+STARTUP: folded")
      (`yearly "#+TITLE: Yearly Journal\n#+STARTUP: folded"))))

(use-package! org-journal
  :config
  (setq org-journal-dir "~/Dropbox/org/journal/"
        org-journal-date-format "%A, %d %B %Y"
        org-journal-file-type 'weekly
        org-journal-enable-agenda-integration t
        org-journal-file-header 'org-journal-file-header-func))

(setq org-pandoc-options-for-docx '((reference-doc . "/home/alancho/Dropbox/templates/cic.docx")))
(setq org-support-shift-select t)

;;;;;;;;;;; Maikol
;;
;;
(setq alancho/bibliography-path "~/Dropbox/org/roam/literature/library.bib")
(setq alancho/pdf-path  "~/Dropbox/Papers/")
(setq alancho/bibliography-notes "~/Dropbox/org/roam/literature/")


(use-package! org-ref
    ;; :init
    ; code to run before loading org-ref
    :config
    (setq
         org-ref-completion-library 'org-ref-ivy-cite
         org-ref-get-pdf-filename-function 'org-ref-get-pdf-filename-helm-bibtex
         org-ref-default-bibliography (list "~/Dropbox/org/roam/literature/library.bib")
         org-ref-bibliography-notes "~/Dropbox/org/roam/literature/bibnotes.org"
         org-ref-note-title-format "* NOTES %y - %t\n :PROPERTIES:\n  :Custom_ID: %k\n  :NOTER_DOCUMENT: %F\n :ROAM_KEY: cite:%k\n  :AUTHOR: %9a\n  :JOURNAL: %j\n  :YEAR: %y\n  :VOLUME: %v\n  :PAGES: %p\n  :DOI: %D\n  :URL: %U\n :END:\n\n"
         org-ref-notes-directory alancho/bibliography-notes
         org-ref-notes-function 'orb-edit-notes
    ))

(after! org-ref
  (setq
   bibtex-completion-notes-path alancho/bibliography-notes
   bibtex-completion-bibliography "~/Dropbox/org/roam/literature/library.bib"
   bibtex-completion-pdf-field "file"
   bibtex-completion-notes-template-multiple-files
   (concat
    "#+TITLE: ${title}\n"
    "#+ROAM_KEY: cite:${=key=}"
    "#+ROAM_TAGS: ${keywords}"
    "#+CREATED:%<%Y-%m-%d-%H-%M-%S>"
    "Time-stamp: <>\n"
    "- tags :: \n"
    "* NOTES \n"
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
)

(use-package! org-roam-bibtex
  :after (org-roam)
  :hook (org-roam-mode . org-roam-bibtex-mode)
  :config
  (setq orb-preformat-keywords
        '("=key=" "title" "url" "file" "author-or-editor" "keywords"))
  (setq orb-templates
        '(("r" "ref" plain (function org-roam-capture--get-point)
           ""
           :file-name "literature/%<%Y-%m-%d-%H-%M-%S>-${slug}"
           :head "#+TITLE: ${=key=}: ${title}
#+ROAM_KEY: ${ref}
#+ROAM_TAGS:
Time-stamp: <>
- tags :: ${keywords}

* ${title}
  :PROPERTIES:
  :Custom_ID: ${=key=}
  :URL: ${url}
  :AUTHOR: ${author-or-editor}
  :NOTER_DOCUMENT: %(orb-process-file-field \"${=key=}\")
  :NOTER_PAGE:
  :END:

"

           :unnarrowed t))))

(use-package! org-noter
  :after (:any org pdf-view)
  :config
  (setq
   ;; The WM can handle splits
   ;;org-noter-notes-window-location 'other-frame
   ;; Please stop opening frames
   ;;org-noter-always-create-frame nil
   ;; I want to see the whole file
   org-noter-hide-other nil
   ;; Everything is relative to the rclone mega
   org-noter-notes-search-path alancho/bibliography-notes
   )
  )
