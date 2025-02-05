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
;; Otras fuentes que me gustan: Ubuntu Mono, Iosevka, Fira Code
(setq doom-font (font-spec :family "DejaVuSansMono" :size 14 :weight 'normal)
      ;; doom-variable-pitch-font (font-spec :family "Iosevka" :size 14)
      )

;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. This is the default:
(setq doom-theme 'doom-one)

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


;; (defun +org-init-capture-defaults-h ()
;;   "Sets up some reasonable defaults, as well as two `org-capture' workflows that
;; I like:

;; 1. The traditional way: invoking `org-capture' directly, via SPC X, or through
;;    the :cap ex command.
;; 2. Through a org-capture popup frame that is invoked from outside Emacs (the
;;    ~/.emacs.d/bin/org-capture script). This can be invoked from qutebrowser,
;;    vimperator, dmenu or a global keybinding."
;;   (setq org-default-notes-file
;;         (expand-file-name +org-capture-notes-file org-directory)
;;         +org-capture-journal-file
;;         (expand-file-name +org-capture-journal-file org-directory)
;;         org-capture-templates
;;         '(("t" "Personal todo" entry
;;            (file+headline +org-capture-todo-file "Inbox")
;;            "* [ ] %?\n%i\n%a" :prepend t)
;;           ("n" "Personal notes" entry
;;            (file+headline +org-capture-notes-file "Inbox")
;;            "* %u %?\n%i\n%a" :prepend t)
;;           ("j" "Journal" entry
;;            (file denote-journal-extras-path-to-new-or-existing-entry)
;;            "* [%(format-time-string \"%H:%M\")] %?\n%i\n%a"
;;            :kill-buffer t
;;            :empty-lines 1)
;;           ;; ("j" "Journal" entry
;;           ;;  (file+olp+datetree +org-capture-journal-file)
;;           ;;  "* %U %?\n%i\n%a" :prepend t)

;;           ;; Will use {project-root}/{todo,notes,changelog}.org, unless a
;;           ;; {todo,notes,changelog}.org file is found in a parent directory.
;;           ;; Uses the basename from `+org-capture-todo-file',
;;           ;; `+org-capture-changelog-file' and `+org-capture-notes-file'.
;;           ("p" "Templates for projects")
;;           ("pt" "Project-local todo" entry  ; {project-root}/todo.org
;;            (file+headline +org-capture-project-todo-file "Inbox")
;;            "* TODO %?\n%i\n%a" :prepend t)
;;           ("pn" "Project-local notes" entry  ; {project-root}/notes.org
;;            (file+headline +org-capture-project-notes-file "Inbox")
;;            "* %U %?\n%i\n%a" :prepend t)
;;           ("pc" "Project-local changelog" entry  ; {project-root}/changelog.org
;;            (file+headline +org-capture-project-changelog-file "Unreleased")
;;            "* %U %?\n%i\n%a" :prepend t)

;;           ;; Will use {org-directory}/{+org-capture-projects-file} and store
;;           ;; these under {ProjectName}/{Tasks,Notes,Changelog} headings. They
;;           ;; support `:parents' to specify what headings to put them under, e.g.
;;           ;; :parents ("Projects")
;;           ("o" "Centralized templates for projects")
;;           ("ot" "Project todo" entry
;;            (function +org-capture-central-project-todo-file)
;;            "* TODO %?\n %i\n %a"
;;            :heading "Tasks"
;;            :prepend nil)
;;           ("on" "Project notes" entry
;;            (function +org-capture-central-project-notes-file)
;;            "* %U %?\n %i\n %a"
;;            :heading "Notes"
;;            :prepend t)
;;           ("oc" "Project changelog" entry
;;            (function +org-capture-central-project-changelog-file)
;;            "* %U %?\n %i\n %a"
;;            :heading "Changelog"
;;            :prepend t)))
;;   )

;; (use-package! org
;;   :preface
;;   (add-hook! 'org-load-hook
;;              #'+org-init-capture-defaults-h)
;;   :config
;;   (setq org-support-shift-select 'always
;;         org-return-follows-link nil
;;         org-hide-emphasis-markers t
;;         org-level-color-stars-only nil
;;         org-replace-disputed-keys t
;;         org-image-actual-width '(500)))

(use-package! org
  :config
  (setq org-support-shift-select 'always
        org-return-follows-link t
        org-hide-emphasis-markers t
        org-level-color-stars-only nil
        org-replace-disputed-keys t
        org-image-actual-width '(500)))


;; Para evitar que el tamaño de la fuente se vea reducida con superscripts o subscripts
(setq font-latex-fontify-script nil)

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
                git-commit-mode-hook
                org-mode-hook)
  #'flyspell-mode)

(ispell-change-dictionary "british" t)
(setq ispell-check-comments nil)

;; Probemos usar avy
(map! :leader
      (:prefix-map ("a" . "avy")
       :desc "Go to char timer" "t" #'avy-goto-char-timer))

;; Mejor manera de usar locate con consult
(setq consult-locate-args "locate --ignore-case --regex")

;; biblio
(setq! citar-bibliography '("~/Dropbox/Papers/library.bib")
       org-cite-global-bibliography '("~/Dropbox/Papers/library.bib")
       citar-library-paths '("~/Dropbox/Papers/")
       citar-notes-paths '("~/Dropbox/notes/"))

(setq citar-open-entry-function #'citar-open-entry-in-zotero)

;; (defun denote--slug-accents (text)
;;   "Return the slug of NODE."
;;   (let ((slug-trim-chars '(;; Combining Diacritical Marks https://www.unicode.org/charts/PDF/U0300.pdf
;;                            768 ; U+0300 COMBINING GRAVE ACCENT
;;                            769 ; U+0301 COMBINING ACUTE ACCENT
;;                            770 ; U+0302 COMBINING CIRCUMFLEX ACCENT
;;                            771 ; U+0303 COMBINING TILDE
;;                            772 ; U+0304 COMBINING MACRON
;;                            774 ; U+0306 COMBINING BREVE
;;                            775 ; U+0307 COMBINING DOT ABOVE
;;                            776 ; U+0308 COMBINING DIAERESIS
;;                            777 ; U+0309 COMBINING HOOK ABOVE
;;                            778 ; U+030A COMBINING RING ABOVE
;;                            779 ; U+030B COMBINING DOUBLE ACUTE ACCENT
;;                            780 ; U+030C COMBINING CARON
;;                            795 ; U+031B COMBINING HORN
;;                            803 ; U+0323 COMBINING DOT BELOW
;;                            804 ; U+0324 COMBINING DIAERESIS BELOW
;;                            805 ; U+0325 COMBINING RING BELOW
;;                            807 ; U+0327 COMBINING CEDILLA
;;                            813 ; U+032D COMBINING CIRCUMFLEX ACCENT BELOW
;;                            814 ; U+032E COMBINING BREVE BELOW
;;                            816 ; U+0330 COMBINING TILDE BELOW
;;                            817 ; U+0331 COMBINING MACRON BELOW
;;                            )))
;;     (cl-flet* ((nonspacing-mark-p (char) (memq char slug-trim-chars))
;;                (strip-nonspacing-marks (s) (string-glyph-compose
;;                                             (apply #'string
;;                                                    (seq-remove #'nonspacing-mark-p
;;                                                                (string-glyph-decompose s)))))
;;                (cl-replace (text pair) (replace-regexp-in-string (car pair) (cdr pair) text)))
;;       (let* ((pairs `(("[^[:alnum:][:digit:]]" . "-") ;; convert anything not alphanumeric
;;                       ("--*" . "-")                   ;; remove sequential underscores
;;                       ("^-" . "")                     ;; remove starting underscore
;;                       ("-$" . "")))                   ;; remove ending underscore
;;              (slug (-reduce-from #'cl-replace (strip-nonspacing-marks text) pairs)))
;;         (downcase slug)))))

;; (defun my/denote-sluggify-title (str)
;;   "Make STR an appropriate slug for title."
;;   (denote--slug-accents (downcase (denote--slug-hyphenate (denote--slug-no-punct str)))))

(use-package! denote
  :config
  (setq denote-directory (expand-file-name "~/Dropbox/denotes/"))
  (setq denote-known-keywords '("moc" "mos" "mor")) ;; Vamos a probar, map of content, map of slides, map or reading
  (setq denote-infer-keywords t)
  (setq denote-sort-keywords t)
  (setq denote-prompts '(title keywords signature))
  (setq denote-excluded-directories-regexp nil)
  (setq denote-excluded-keywords-regexp nil)
  (setq denote-date-prompt-use-org-read-date t)
  (setq denote-backlinks-show-context t)
  (setq denote-dired-directories (list denote-directory))
  ;; (setq denote-file-name-slug-functions
  ;;       '((title . my/denote-sluggify-title)
  ;;         (signature . denote-sluggify-signature)
  ;;         (keywords . denote-sluggify-keywords)))
  :hook
  (dired-mode . denote-dired-mode-in-directories)
  :bind
  (("C-c d n" . denote-create-note)
   ("C-c d f" . denote-open-or-create)
   ("C-c d j" . denote-date)
   ("C-c d i" . denote-link-or-create)
   ("C-c d l" . denote-find-link)
   ("C-c d b" . denote-find-backlink)
   ("C-c d D" . denote-org-dblock-insert-links)
   ("C-c d r" . denote-rename-file-using-front-matter)
   ("C-c d R" . denote-rename-file)
   ("C-c d k" . denote-keywords-add)
   ("C-c d K" . denote-keywords-remove)))

(defun open-my-denote-directory ()
  (interactive)
  (find-file "~/Dropbox/denotes"))

(global-set-key (kbd "<f12>") 'open-my-denote-directory)


;; ;; Define a read-only directory class
;; (dir-locals-set-class-variables 'read-only
;;  '((nil . ((buffer-read-only . t)))))

;; ;; Esto es para ver si puedo abrir las notas de denote siempre en read-only, para no meter la pata
;; ;; Associate directories with the read-only class
;; (dolist (dir (list "~/Dropbox/notes-testing/"))
;;   (dir-locals-set-directory-class (file-truename dir) 'read-only))

(use-package! citar-denote
  :after denote
  :init
  (citar-denote-mode)
  :config
  (setq citar-denote-title-format "author-year-title")
  (setq citar-denote-subdir nil)
  (setq citar-denote-title-format-authors 2)
  :bind
  (("C-c d c" . citar-denote-open-reference-entry)))

;; (use-package! consult-notes
;;   :after denote
;;   :hook
;;   (dired-mode . consult-notes-denote-mode)
;;   :bind
;;   (("C-c d d" . consult-notes)))

(after! consult-notes
  (consult-notes-denote-mode))

(map! :map doom-leader-notes-map
      "b" #'citar-insert-citation)

;; (use-package! org-transclusion
;;   :after org
;;   :init
;;   (map!
;;    :map global-map "<f12>" #'org-transclusion-add
;;    :leader
;;    :prefix "n"
;;    :desc "Org Transclusion Mode" "t" #'org-transclusion-mode))

(use-package! gptel
  :config
  (defun ads/read-openai-key ()
    (with-temp-buffer
      (insert-file-contents "~/key.txt")
      (string-trim (buffer-string))))
  (setq gptel-model 'gpt-4o
        gptel-stream t
        gptel-default-mode 'org-mode
        gptel-api-key #'ads/read-openai-key
        ;; gptel--system-message "You are a large language model living in Emacs and a helpful assistant. Respond concisely. Please provide all responses in Australian English."
        ;; gptel--rewrite-message "You are a prose editor. Rewrite the following text to be more professional, ensuring it's still clear and easily understandable, in Australian English."
        gptel-directives
        '((default . "You are a large language model living in Emacs and a helpful assistant. Respond concisely in Australian English.")
          (programming . "You are a large language model and a careful programmer. Provide code and only code as output without any additional text, prompt or note.")
          (writing . "You are a large language model and a writing assistant. Respond concisely in Australian English.")
          (chat . "You are a large language model and a conversation partner. Respond concisely in Australian English.")
          (aussie . "G'day! You're a fair dinkum Aussie assistant living in Emacs. Respond in Australian English, using local slang and expressions where appropriate, mate."))))


(use-package! org-format
  :hook (org-mode . org-format-on-save-mode))

(use-package! oxr)

(after! org
  (setq org-todo-keywords
        '((sequence "LEER(l)" "|" "LEIDO(L)" "CANCELLED(c)"))))

;; Prefiero que la fuente de los headings en org no sea bold
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(org-level-1 ((t (:inherit outline-1 :extend t :weight normal))))
 '(org-level-2 ((t (:inherit outline-2 :extend t :weight normal))))
 '(org-level-3 ((t (:inherit outline-2 :extend t :weight normal))))
 '(org-level-4 ((t (:inherit outline-2 :extend t :weight normal))))
 '(org-level-5 ((t (:inherit outline-2 :extend t :weight normal))))
 '(org-level-6 ((t (:inherit outline-2 :extend t :weight normal))))
 '(org-level-7 ((t (:inherit outline-2 :extend t :weight normal))))
 '(org-level-8 ((t (:inherit outline-2 :extend t :weight normal))))
 '(org-level-9 ((t (:inherit outline-2 :extend t :weight normal))))
 '(org-level-10 ((t (:inherit outline-2 :extend t :weight normal))))
 '(org-level-11 ((t (:inherit outline-2 :extend t :weight normal))))
 '(org-level-12 ((t (:inherit outline-2 :extend t :weight normal)))))

(defvar original-frame-font nil
  "Variable to store the original frame font before toggling.")

(defvar original-buffer-face-mode-face nil
  "Variable to store the original buffer-face-mode-face before toggling.")

(defun toggle-olivetti-and-set-iosevka ()
  "Toggle between activating and deactivating Olivetti mode and set Iosevka font."
  (interactive)
  ;; Check if Olivetti mode is active
  (if (bound-and-true-p olivetti-mode)
      ;; Deactivate Olivetti mode
      (progn
        (olivetti-mode -1)
        ;; Restore the original font and buffer-face-mode-face
        (when original-frame-font
          (set-frame-font original-frame-font t t)
          (setq original-frame-font nil))
        (when original-buffer-face-mode-face
          (setq buffer-face-mode-face original-buffer-face-mode-face)
          (buffer-face-mode 0)))  ;; Turn off buffer-face-mode
    ;; Activate Olivetti mode
    (progn
      ;; Save the original font and buffer-face-mode-face
      (setq original-frame-font (face-attribute 'default :font))
      (setq original-buffer-face-mode-face buffer-face-mode-face)
      ;; Set Iosevka font locally for the buffer
      (setq buffer-face-mode-face '(:family "Iosevka Comfy" :height 120))
      (buffer-face-mode 1)
      ;; Activate Olivetti mode
      (olivetti-mode 1)
      (setq olivetti-body-width 150))))

(map!
 :leader
 :prefix "t"
 :desc "Olivetti" "o" 'toggle-olivetti-and-set-iosevka)

(after! writeroom-mode
  (setq +zen-text-scale 1
        +zen-mixed-pitch-modes '(adoc-mode rst-mode)))

;; Para que me asocie apsim a xml-mode
(add-to-list 'auto-mode-alist '("\\.apsim\\'" . xml-mode))
(add-to-list 'auto-mode-alist '("\\.apsimx\\'" . json-mode))

;; Y para que asocie Stan con c++-mode
(add-to-list 'auto-mode-alist '("\\.stan\\'" . c++-mode))

;; A ver cómo anda ChatGPT que le pregunté cómo hacer esto
(defun create-temporary-org-file ()
  "Create and save a new temporary Org mode file in /tmp directory with a unique name."
  (interactive)
  (let* ((timestamp (format-time-string "%Y%m%d%H%M%S"))
         (file-name (concat "temp_org_" timestamp ".org"))
         (file-path (concat "/tmp/" file-name)))
    (switch-to-buffer (find-file-noselect file-path))
    (org-mode)
    (toggle-olivetti-and-set-iosevka)
    (save-buffer)
    (message (concat "Temporary Org file created and saved as " file-path))))

(setq global-hl-line-modes nil)
;; (global-hl-line-mode -1)

(add-to-list 'exec-path "/home/alancho/.cabal/bin")

(after! org
  (set-popup-rule! "^\\*Org Src" :ignore t))

(after! org
  (setq org-src-window-setup 'current-window)
  (setq org-descriptive-links t))

(defun create-temporary-r-file ()
  "Create and save a new temporary R file in /tmp directory with a unique name."
  (interactive)
  (let* ((timestamp (format-time-string "%Y%m%d%H%M%S"))
         (file-name (concat "temp_R_" timestamp ".R"))
         (file-path (concat "/tmp/" file-name)))
    (switch-to-buffer (find-file-noselect file-path))
    (setq-local auto-mode-alist (cons '("\\.R$" . ess-mode) auto-mode-alist))
    (insert "require(tidyverse)\n")
    (save-buffer)
    (message (concat "Temporary R file created and saved as " file-path))))

(after! figlet
  (setq figlet-default-font "banner"))

(setq org-attach-store-link-p 'attached)

;; (add-to-list 'org-latex-packages-alist '("" "moderncv" t))

(with-eval-after-load 'ox-latex
  (add-to-list 'org-latex-classes
               '("articulo"
                 "\\documentclass{article}
                  [NO-DEFAULT-PACKAGES]
                  [NO-PACKAGES]
                  [EXTRA]"
                 ("\\section{%s}" . "\\section*{%s}")
                 ("\\subsection{%s}" . "\\subsection*{%s}")
                 ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
                 ("\\paragraph{%s}" . "\\paragraph*{%s}")
                 ("\\subparagraph{%s}" . "\\subparagraph*{%s}")))
  (add-to-list 'org-latex-classes
               '("moderncv"
                 "\\documentclass[8pt,a4paper,sans]{moderncv}"
                 ("\\section{%s}" . "\\section*{%s}")
                 ("\\subsection{%s}" . "\\subsection*{%s}")
                 ("\\subsubsection{%s}" . "\\subsubsection*{%s}"))))

;; Para exportar siempre fuera del directorio
(defun my-org-export-output-file-name (orig-fun extension &optional subtreep pub-dir)
  (let ((pub-dir "~/Downloads"))
    (funcall orig-fun extension subtreep pub-dir)))

(advice-add 'org-export-output-file-name :around #'my-org-export-output-file-name)

;; ######
;; #     # #   # ##### #    #  ####  #    #
;; #     #  # #    #   #    # #    # ##   #
;; ######    #     #   ###### #    # # #  #
;; #         #     #   #    # #    # #  # #
;; #         #     #   #    # #    # #   ##
;; #         #     #   #    #  ####  #    #

(after! company
  (setq company-idle-delay 0.0
        company-minimum-prefix-length 1))

(use-package! company-box
  :hook (company-mode . company-box-mode))

(after! lsp-mode
  (setq lsp-headerline-breadcrumb-enable nil
        lsp-enable-snippet t))

;; Enable LSP for Python automatically
(add-hook 'python-mode-hook #'lsp!)

(after! flycheck
  (setq flycheck-python-pyright-executable "pyright"))

(setq conda-anaconda-home (expand-file-name "~/miniconda3"))

(use-package! dap-mode
  :after lsp-mode
  :config
  (require 'dap-python)
  (setq dap-python-executable "python3"))

(use-package! blacken
  :hook (python-mode . blacken-mode))

(setq gc-cons-threshold 100000000) ;; Increase garbage collection threshold
(setq read-process-output-max (* 1024 1024)) ;; Increase LSP performance

;; (defun my/python-shell-send-buffer-with-repl-check ()
;;   "Start Python REPL if not already running, then send the buffer."
;;   (interactive)
;;   (unless (comint-check-proc "*Python*")
;;     (run-python))
;;   (python-shell-send-buffer))

;; ;; Add the keybinding after python-mode is loaded
;; (with-eval-after-load 'python
;;   (define-key python-mode-map (kbd "C-c C-c") 'my/python-shell-send-buffer-with-repl-check))

(defun my/python-shell-send-buffer-with-repl-check-and-split ()
  "Start Python REPL if not already running, split window vertically,
show REPL on the right, and send the buffer."
  (interactive)
  ;; Check if a Python REPL is running
  (unless (comint-check-proc "*Python*")
    ;; If not, start a Python REPL
    (run-python))
  ;; Split the window if the REPL is not visible
  (unless (get-buffer-window "*Python*")
    (split-window-right)                     ;; Split the window vertically
    (other-window 1)                         ;; Move to the new window
    (switch-to-buffer "*Python*")            ;; Switch to the Python REPL buffer
    (other-window -1))                       ;; Move back to the script window
  ;; Send the buffer to the REPL
  (python-shell-send-buffer))

;; Add the keybinding after python-mode is loaded
(with-eval-after-load 'python
  (define-key python-mode-map (kbd "C-c C-c") 'my/python-shell-send-buffer-with-repl-check-and-split))
