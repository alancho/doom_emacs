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

(setq doom-font (font-spec :family "DejaVuSansMono" :size 14 :weight 'normal)
      doom-variable-pitch-font (font-spec :family "Iosevka" :size 14))

;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. This is the default:
(setq doom-theme 'doom-vibrant)

;; (set-face-attribute 'default nil :height 105 :family "dejavu" :weight 'normal :width 'normal)

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
  :hook (org-mode-hook . org-appear-mode)
  :config
  (setq org-support-shift-select t
        org-return-follows-link t
        org-hide-emphasis-markers t
        org-level-color-stars-only nil
        org-replace-disputed-keys t
        org-startup-with-inline-images t
        org-image-actual-width '(400))
  )

;; Para evitar que el tamaño de la fuente se vea reducida con superscripts o subscripts
(setq font-latex-fontify-script nil)

(use-package! org-roam
  :config
  (setq org-support-shift-select t
        org-return-follows-link t
        org-roam-db-autosync-mode t
        org-replace-disputed-keys t
        org-roam-dailies-capture-templates
        '(("d" "default" entry "* %<%I:%M %p>\n%?"
        ;; '(("d" "default" entry "* %?"
           :if-new (file+head "%<%Y-%m-%d>.org" "#+title: %<%A, %e %B %Y>\n")))))

(use-package! websocket
  :after org-roam)

(use-package! org-roam-ui
  :after org-roam
  ;; :hook (after-init . org-roam-ui-mode)
  :config
  (setq org-roam-ui-sync-theme t
        org-roam-ui-follow t
        org-roam-ui-update-on-save t
        org-roam-ui-open-on-start t))

(global-set-key (kbd "<f5>") #'polymode-toggle-chunk-narrowing)
(global-set-key (kbd "<f7>") #'unfill-toggle)
(global-set-key (kbd "<f8>") #'org-edit-special)
(global-set-key (kbd "<f12>") #'org-roam-dailies-capture-today)

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

(remove-hook! '(markdown-mode-hook
                TeX-mode-hook
                rst-mode-hook
                mu4e-compose-mode-hook
                message-mode-hook
                git-commit-mode-hook
                org-mode-hook)
  #'flyspell-mode)

(ispell-change-dictionary "british" t)
(setq ispell-check-comments nil)

;; Mejor manera de usar locate con consult
(setq consult-locate-args "locate --ignore-case --regex")

;; Copiado de tocosaur: https://tecosaur.github.io/emacs-config/config.html
(use-package! oc-csl-activate
  :after oc
  :config
  (setq org-cite-csl-activate-use-document-style t)
  (defun +org-cite-csl-activate/enable ()
    (interactive)
    (setq org-cite-activate-processor 'csl-activate)
    (add-hook! 'org-mode-hook '((lambda () (cursor-sensor-mode 1)) org-cite-csl-activate-render-all))
    (defadvice! +org-cite-csl-activate-render-all-silent (orig-fn)
      :around #'org-cite-csl-activate-render-all
      (with-silent-modifications (funcall orig-fn)))
    (when (eq major-mode 'org-mode)
      (with-silent-modifications
        (save-excursion
          (goto-char (point-min))
          (org-cite-activate (point-max)))
        (org-cite-csl-activate-render-all)))
    (fmakunbound #'+org-cite-csl-activate/enable)))

(use-package! citar
  :config
  (setq
   org-cite-global-bibliography '("~/Dropbox/Papers/library.bib")
   org-cite-insert-processor 'citar
   org-cite-follow-processor 'citar
   org-cite-activate-processor 'citar
   citar-bibliography org-cite-global-bibliography
   citar-library-paths '("~/Dropbox/Papers/")
   citar-at-point-function 'embark-act
   citar-symbols
   `((file ,(all-the-icons-faicon "file-o" :face 'all-the-icons-green :v-adjust -0.1) . " ")
     (note ,(all-the-icons-material "speaker_notes" :face 'all-the-icons-blue :v-adjust -0.3) . " ")
     (link ,(all-the-icons-octicon "link" :face 'all-the-icons-orange :v-adjust 0.01) . " "))))

(after! citar-org-roam
  (setq citar-org-roam-note-title-template "${title}\n#+subtitle: ${author editor} (${year})"))

(after! oc-csl
  (setq org-cite-csl-styles-dir "~/Dropbox/templates/csl"))

(after! oc
  (setq org-cite-export-processors '((t csl))))

(use-package! org-format
  :hook (org-mode . org-format-on-save-mode))

(map! :after citar
      :map doom-leader-notes-map
      "b" #'citar-insert-citation)

(use-package! oxr)

(after! ox-latex
  (add-to-list 'org-latex-classes
               '("org-plain-latex"
                 "\\documentclass{article}
           [NO-DEFAULT-PACKAGES]
           [PACKAGES]
           [EXTRA]"
                 ("\\section{%s}" . "\\section*{%s}")
                 ("\\subsection{%s}" . "\\subsection*{%s}")
                 ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
                 ("\\paragraph{%s}" . "\\paragraph*{%s}")
                 ("\\subparagraph{%s}" . "\\subparagraph*{%s}"))))

(use-package! org-transclusion
  :after org
  :init
  (map!
   :map global-map "<f12>" #'org-transclusion-add
   :leader
   :prefix "n"
   :desc "Org Transclusion Mode" "t" #'org-transclusion-mode))

(defun ads/read-openai-key ()
  (with-temp-buffer
    (insert-file-contents "~/key.txt")
    (string-trim (buffer-string))))

(use-package! gptel
  :config
  (setq gptel-model "gpt-3.5-turbo"
        gptel-playback t
        gptel-default-mode 'org-mode
        gptel-api-key #'ads/read-openai-key))
