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
(setq org-roam-directory "~/Dropbox/org/roam")
(add-hook 'after-init-hook 'org-roam-mode)
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
	;; ess-eval-visibly-p nil
        ess-use-flymake nil
        ;; ess-eval-visibly 'nowait
	ess-eval-visibly t
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

(use-package! org-pomodoro
  :config
  (setq org-pomodoro-start-sound-p t
        org-pomodoro-killed-sound-p t
        org-pomodoro-ask-upon-killing t
        org-pomodoro-keep-killed-pomodoro-time t
        org-pomodoro-start-sound (expand-file-name "~/Dropbox/templates/sonidos/percussion-10.wav")
        org-pomodoro-finished-sound (expand-file-name "~/Dropbox/templates/sonidos/percussion-28.wav")
        org-pomodoro-short-break-sound (expand-file-name "~/Dropbox/templates/sonidos/percussion-12.wav")
        org-pomodoro-long-break-sound (expand-file-name "~/Dropbox/templates/sonidos/percussion-50.wav")
        org-pomodoro-killed-sound (expand-file-name"~/Dropbox/templates/sonidos/percussion-10.wav")
        ))

(after! org (setq org-agenda-files (append (file-expand-wildcards "~/Dropbox/org/*.org")
                                           (file-expand-wildcards "~/Dropbox/org/roam/daily/*.org"))))
                                           ;; (file-expand-wildcards "~/projects/orgmode/gtd/*/*.org"))))
(after! ivy
  (setq ivy-use-virtual-buffers t))

(after! org
  (setq org-todo-keywords
        '((sequence
           "TODO(t)"  ; A task that needs doing & is ready to do
           "PROJ(p)"  ; A project, which usually contains other tasks
           "STRT(s)"  ; A task that is in progress
           "WAIT(w)"  ; Something external is holding up this task
           "HOLD(h)"  ; This task is paused/on hold because of me
           "|"
           "DONE(d)"  ; Task successfully completed
           "KILL(k)") ; Task was cancelled, aborted or is no longer applicable
          ))
  (setq org-capture-templates
        '(("t" "Personal todo" entry
           (file+headline +org-capture-todo-file "Inbox")
           "* TODO %?\n%i\n%a" :prepend t)
          ("n" "Personal notes" entry
           (file+headline +org-capture-notes-file "Inbox")
           "* %u %?\n%i\n%a" :prepend t)
          ("j" "Journal" entry
           (file+olp+datetree +org-capture-journal-file)
           "* %U %?\n%i\n%a" :prepend t))))

;; (use-package! org-ref
;;   :after org-roam
;;   ;; :init
;;   ;; (setq org-ref-completion-library 'org-ref-ivy-cite)
;;   :config
;;   (setq org-ref-default-bibliography "/home/alancho/Dropbox/org/library.bib"
;;         reftex-default-bibliography '("/home/alancho/Dropbox/org/library.bib")
;;         org-ref-bibliography-notes "/home/alancho/Dropbox/org/roam/bibnotes.org"
;;         org-ref-notes-directory "/home/alancho/Dropbox/org/roam/" ;; org-ref also knows where the notes are stored, so there must be some direct way to open them from a cite-link!
;;         org-ref-notes-function 'orb-edit-notes
;;         org-ref-get-pdf-filename-function 'org-ref-get-pdf-filename-helm-bibtex))

(after! org-ref
  (setq org-ref-default-bibliography (list "/home/alancho/Dropbox/org/library.bib")))

;; (use-package! ivy-bibtex
;;   :after org-ref
;;   :config
;;   (setq bibtex-completion-notes-path "/home/alancho/Dropbox/org/roam/"
;;         bibtex-completion-bibliography "/home/alancho/Dropbox/org/library.bib"
;;         bibtex-completion-pdf-field "file"
;;         ;; ivy-bibtex-default-action 'ivy-bibtex-insert-citation
;;         ))

;; (use-package! org-roam-bibtex
;;   :after org-roam
;;   :hook (org-roam-mode . org-roam-bibtex-mode)
;;   :bind (:map org-mode-map
;;          (("<f8>" . orb-note-actions)))
;;   :config
;;   (setq orb-preformat-keywords
;;         '("=key=" "title" "url" "file" "author-or-editor" "keywords"))
;;   (setq orb-templates
;;         '(("r" "ref" plain (function org-roam-capture--get-point)
;;            ""
;;            :file-name "${slug}"
;;            :head "#+TITLE: ${title}
;; #+ROAM_KEY: ${ref}
;; - tags ::
;; - keywords :: ${keywords}
;; * ${title}
;; :PROPERTIES:
;; :Custom_ID: ${citekey}
;; :URL: ${url}
;; :AUTHOR: ${author-or-editor}
;; :NOTER_DOCUMENT: %(orb-process-file-field \"${citekey}\")
;; :NOTER_PAGE:
;; :END:
;; "
;;            :unnarrowed t))))

;; (use-package! org-noter
;;   :after (:any org pdf-view)
;;   :config
;;   (setq
;;    org-noter-notes-window-location 'other-frame ;; The WM can handle splits
;;    org-noter-always-create-frame nil ;; Please stop opening frames
;;    org-noter-hide-other nil ;; I want to see the whole file
;;    org-noter-notes-search-path (list org-roam-directory))) ;; Everything is relative to the main notes file

;; (use-package! org-roam
;;   :hook ((after-init . org-roam-mode))
;;   :custom
;;   (setq org-roam-directory "/home/alancho/Dropbox/org/roam/"))

;; (use-package! org-roam
;;   :commands (org-roam-insert org-roam-find-file org-roam-switch-to-buffer org-roam)
;;   :hook
;;   (after-init . org-roam-mode)
;;   :init
;;   (map! :leader
;;         :prefix "n"
;;         :desc "org-roam" "l" #'org-roam
;;         :desc "org-roam-insert" "i" #'org-roam-insert
;;         :desc "org-roam-switch-to-buffer" "b" #'org-roam-switch-to-buffer
;;         :desc "org-roam-find-file" "f" #'org-roam-find-file
;;         :desc "org-roam-show-graph" "g" #'org-roam-graph-show
;;         :desc "org-roam-capture" "c" #'org-roam-capture
;;         :desc "org-roam-dailies-capture-today" "j" #'org-roam-dailies-capture-today)
;;   (setq org-roam-directory "/home/alancho/Dropbox/org/roam/"
;;         ;; org-roam-db-gc-threshold most-positive-fixnum
;;         ;; org-roam-graph-exclude-matcher "private"
;;         org-roam-tag-sources '(prop last-directory)
;;         org-id-link-to-org-use-id t)
;;   :config
;;   (setq org-roam-capture-templates
;;         '(("d" "default" plain (function org-roam--capture-get-point)
;;            "%?"
;;            :file-name "${slug}"
;;            :head "#+title: ${title}\n"
;;            :immediate-finish t
;;            :unnarrowed t)
;;           ("p" "private" plain (function org-roam-capture--get-point)
;;            "%?"
;;            :file-name "private/${slug}"
;;            :head "#+title: ${title}\n"
;;            :immediate-finish t
;;            :unnarrowed t)))
;;   (setq org-roam-capture-ref-templates
;;         '(("r" "ref" plain (function org-roam-capture--get-point)
;;            "%?"
;;            :file-name "${slug}"
;;            :head "#+roam_key: ${ref}
;; #+roam_tags: website
;; #+title: ${title}
;; - source :: ${ref}"
;;            :unnarrowed t)))
;;   (setq org-roam-dailies-directory "daily/")
;;   (setq org-roam-dailies-capture-templates
;;       '(("d" "default" entry
;;          #'org-roam-capture--get-point
;;          "* %?"
;;          :file-name "daily/%<%Y-%m-%d>"
;;          :head "#+title: %<%Y-%m-%d>\n\n"))))

(after! org-noter
  org-noter-doc-split-fraction '(0.57 0.43))

(use-package! org-roam-server)

(use-package! org-roam-bibtex
  :after (org-roam)
  :hook (org-roam-mode . org-roam-bibtex-mode)
  :config
  (setq org-roam-bibtex-preformat-keywords
   '("=key=" "title" "url" "file" "author-or-editor" "keywords"))
  (setq orb-templates
        `(("r" "ref" plain (function org-roam-capture--get-point)
           ""
           :file-name "lit/${slug}"
           :head ,(concat
                   "#+title: ${=key=}: ${title}\n"
                   "#+roam_key: ${ref}\n\n"
                   "* ${title}\n"
                   "  :PROPERTIES:\n"
                   "  :Custom_ID: ${=key=}\n"
                   "  :URL: ${url}\n"
                   "  :AUTHOR: ${author-or-editor}\n"
                   "  :NOTER_DOCUMENT: %(orb-process-file-field \"${=key=}\")\n"
                   "  :NOTER_PAGE: \n"
                   "  :END:\n")
           :unnarrowed t))))

(use-package! bibtex-completion
  :config
  (setq bibtex-completion-notes-path "~/Dropbox/org/roam/lit/"
        bibtex-completion-bibliography "~/Dropbox/org/library.bib"
        bibtex-completion-pdf-field "file"
        bibtex-completion-notes-template-multiple-files
         (concat
          "#+title: ${title}\n"
          "#+roam_key: cite:${=key=}\n"
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
          )))
