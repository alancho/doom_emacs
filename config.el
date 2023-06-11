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
(setq doom-font (font-spec :family "Ubuntu Mono" :size 14 :weight 'normal)
      doom-variable-pitch-font (font-spec :family "sans" :size 13))

;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. This is the default:
(setq doom-theme 'doom-vibrant)

;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
(setq org-directory "~/Dropbox/org")

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
  (defun then_R_operator_inline ()
    (interactive)
    (just-one-space 1)
    (insert "%>%")
    (just-one-space 1))
  (defun then_ggplot_plus ()
    (interactive)
    (just-one-space 1)
    (insert "+")
    (reindent-then-newline-and-indent))
  (defun then_reticulate_dollar ()
    (interactive)
    (insert "$"))
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
        ess-eval-visibly 'nowait
	ess-default-style 'RStudio
	fill-column 72
	comment-auto-fill-only-comments t)
  :bind (:map ess-mode-map
	 ("C-S-<return>" . 'then_R_operator_inline)
	 ("C-<return>" . 'then_R_operator)
	 ("M-<return>" . 'then_ggplot_plus)
	 ("_" . 'ess-insert-assign)
	 ("S-<return>" . 'ess-eval-region-or-function-or-paragraph-and-step)
	 ("s-<return>" . 'then_reticulate_dollar)
         :map inferior-ess-r-mode-map
	 ("C-S-<return>" . 'then_R_operator_inline)
	 ("C-<return>" . 'then_R_operator)
	 ("M-<return>" . 'then_ggplot_plus)
	 ("_" . 'ess-insert-assign)
         ("S-<return>" . 'ess-eval-region-or-function-or-paragraph-and-step)
         ("s-<return>" . 'then_reticulate_dollar)
         )
  )

(setq projectile-track-known-projects-automatically nil)

(setq company-global-modes
      '(not ess-r-mode
            inferior-ess-r-mode
            emacs-lisp-mode
            eshell-mode
            markdown-mode
            org-mode
            shell-mode
            text-mode
            latex-mode))

(add-hook 'markdown-mode-hook 'pandoc-mode)

(use-package! org
  :config
  (setq org-support-shift-select t
        org-return-follows-link t
        org-image-actual-width '(800))
  )

;; Para evitar que el tamaño de la fuente se vea reducida con superscripts o subscripts
(setq font-latex-fontify-script nil)

;; (map! :map doom-leader-notes-map
;;       "b" #'citar-insert-citation)
;;       ;; "b" #'org-cite-insert)
;;       ;; "b" #'citar-open-notes)

;; (use-package! citar
;;   :config
;;   (setq
;;    org-cite-global-bibliography '("~/Dropbox/Papers/library.bib")
;;    org-cite-csl-styles-dir "~/Dropbox/templates/csl"
;;    org-cite-insert-processor 'citar
;;    org-cite-follow-processor 'citar
;;    org-cite-activate-processor 'citar
;;    citar-bibliography org-cite-global-bibliography
;;    citar-notes-paths '("~/Dropbox/org/roam/literature")
;;    citar-at-point-function 'embark-act
;;    citar-templates
;;    '((main . "${author editor:25}   ${date year issued:4}   ${title:40}")
;;      (suffix . "   ${=key= id:40}   ${=type=:12}")
;;      (preview . "${author editor} (${year issued date}) ${title}, ${journal journaltitle publisher container-title collection-title}.\n")
;;      (note . "Notas de ${=key=}: ${title}, ${journal}\n\n* Abstract\n\n#+begin_quote\n${abstract}\n#+end_quote\n\n* Quotes\n\n* Fleeting notes\n# Lo que te surja al leerlo\n\n* Literature notes\n# En tus propias palabras\n")))
;;   )

;; biblio
(after! citar
  (setq! citar-bibliography '("~/Dropbox/Papers/library.bib"))
  (setq! citar-library-paths '("~/Dropbox/Papers"))
  (setq! citar-notes-paths '("~/Dropbox/notes"))
  )

;; (setq!
;;    citar-bibliography '("~/Dropbox/Papers/library.bib")
;;    org-cite-csl-styles-dir "~/Dropbox/templates/csl"
;;    ;; citar-notes-paths '("~/Dropbox/org/roam/literature")
;;    )
;;    ;; citar-templates
;;    ;; '(;; (main . "${author editor:25}   ${date year issued:4}   ${title:40}")
;;    ;;   ;; (suffix . "   ${=key= id:40}   ${=type=:12}")
;;    ;;   ;; (preview . "${author editor} (${year issued date}) ${title}, ${journal journaltitle publisher container-title collection-title}.\n")
;;    ;;   (note . "Notas de ${=key=}: ${title}, ${journal}\n\n* Abstract\n\n#+begin_quote\n${abstract}\n#+end_quote\n\n* Quotes\n\n* Fleeting notes\n# Lo que te surja al leerlo\n\n* Literature notes\n# En tus propias palabras\n"))
;;    ;; )

(global-set-key (kbd "<f5>") #'polymode-toggle-chunk-narrowing)
(global-set-key (kbd "<f7>") #'unfill-toggle)
(global-set-key (kbd "<f8>") #'org-edit-special)

;; Aliases
(set-eshell-alias! "us" "sudo apt-get update && sudo apt-get upgrade && sudo apt-get clean"
                   "up" "conda update conda && conda update --all"
                   "ll" "ls -lha"
                   "ur" "r update-r-packages.R"
                   "un" "unison sandisco"
                   "ud" "doom sync && doom upgrade"
                   "ds" "dropbox status")
(after! ispell
  ;; Don't spellcheck org blocks
  (pushnew! ispell-skip-region-alist
            '(":\\(PROPERTIES\\|LOGBOOK\\):" . ":END:")
            '("#\\+BEGIN_SRC" . "#\\+END_SRC")
            '("#\\+BEGIN_EXAMPLE" . "#\\+END_EXAMPLE")
            '("#\\+BEGIN_QUOTE" . "#\\+END_QUOTE")
            '("#\\+begin_src" . "#\\+end_src")
            '("#\\+begin_example" . "#\\+end_example")
            '("#\\+begin_quote" . "#\\+end_quote")
            )
  )

;; Solamente quiero que se active en org-mode-hook
(remove-hook! '(markdown-mode-hook
                TeX-mode-hook
                rst-mode-hook
                mu4e-compose-mode-hook
                message-mode-hook
                git-commit-mode-hook)
  #'flyspell-mode)

(ispell-change-dictionary "british" t)
(setq ispell-check-comments nil)

;; Probemos usar avy
(map! :leader
      (:prefix-map ("a" . "avy")
       :desc "Go to char timer" "t" #'avy-goto-char-timer))

;; Mejor manera de usar locate con consult
(setq consult-locate-args "locate --ignore-case --regex")

;; No más ir a una website para generar ASCII comments!
(setq figlet-default-font "georgia11")
(setq figlet-options '("-w 200"))

(use-package! denote
  :config
  (setq denote-directory (expand-file-name "~/Dropbox/notes/"))
  (setq denote-known-keywords '("bayesian" "cropscience" "ai"))
  (setq denote-infer-keywords t)
  (setq denote-sort-keywords t)
  (setq denote-file-type 'markdown-yaml) ; Org is the default, set others here
  (setq denote-prompts '(title keywords))
  (setq denote-excluded-directories-regexp nil)
  (setq denote-excluded-keywords-regexp nil)
  (setq denote-date-prompt-use-org-read-date t)
  (setq denote-backlinks-show-context t)
  (add-hook 'find-file-hook #'denote-link-buttonize-buffer)
  (add-hook 'dired-mode-hook #'denote-dired-mode-in-directories)
)
