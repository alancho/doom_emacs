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

(use-package! denote
  :config
  (setq denote-directory (expand-file-name "~/Dropbox/denotes/"))
  ;; (setq denote-directory (expand-file-name "~/Downloads/denote-sim/"))
  (setq denote-known-keywords '("moc" "mos" "mor")) ;; Vamos a probar, map of content, map of slides, map or reading
  (setq denote-infer-keywords t)
  (setq denote-sort-keywords t)
  (setq denote-prompts '(title keywords signature))
  (setq denote-excluded-directories-regexp nil)
  (setq denote-excluded-keywords-regexp nil)
  (setq denote-date-prompt-use-org-read-date t)
  (setq denote-backlinks-show-context t)
  (setq denote-journal-extras-title-format 'day-date-month-year)
  (setq denote-journal-extras-directory nil)
  (setq denote-dired-directories (list denote-directory))
  :hook
  (dired-mode . denote-dired-mode-in-directories)
  :bind
  (("C-c n d n" . denote-create-note)
   ("C-c n d f" . denote-open-or-create)
   ("C-c n d j" . denote-date)
   ("C-c n d i" . denote-link-or-create)
   ("C-c n d I" . denote-link-insert-links-matching-regexp)
   ("C-c n d l" . denote-find-link)
   ("C-c n d b" . denote-find-backlink)
   ("C-c n d D" . denote-org-dblock-insert-links)
   ("C-c n d r" . denote-rename-file-using-front-matter)
   ("C-c n d R" . denote-rename-file)
   ("C-c n d k" . denote-keywords-add)
   ("C-c n d K" . denote-keywords-remove)))

(setq org-capture-templates
      '(("t" "Personal todo" entry
         (file+headline denote-journal-extras-path-to-new-or-existing-entry "Tasks")
         ;; "* TODO [%(format-time-string \"%H:%M\")] %?\n%i\n%a" :prepend t)
         "* TODO %U %?\n%i\n%a" :prepend t)
        ("n" "Personal note" entry
         (file+headline denote-journal-extras-path-to-new-or-existing-entry "Notes")
         "* %U %?\n%i\n%a" :prepend t)))

(defun jab/denote-add-to-agenda-files (keyword)
  "Append list of files containing 'keyword' to org-agenda-files."
  (interactive "sEnter keyword: ")
  (let ((files (directory-files "~/Dropbox/denotes/" t keyword)))
    ;; (dolist (file files)
    ;;   (unless (member file org-agenda-files)
    ;;     (add-to-list 'org-agenda-files file)))))
    (setq org-agenda-files
          (append (remove nil
                          (mapcar (lambda (file)
                                    (unless (member file org-agenda-files)
                                      file))
                                  files))
                  org-agenda-files))))

(jab/denote-add-to-agenda-files "_journal\\|_project")

;; ;; Add all Denote files tagged as "project" to org-agenda-files
;; (defun salan/denote-add-to-agenda-files (keyword)
;;   "Append list of files containing 'keyword' to org-agenda-files"
;;   (interactive)
;;   (setq org-agenda-files (append org-agenda-files (directory-files denote-directory t keyword))))

;; (salan/denote-add-to-agenda-files "_project")

(defun open-my-denote-directory ()
  (interactive)
  (find-file "~/Dropbox/denotes"))
;; (find-file "~/Downloads/denote-sim"))

(global-set-key (kbd "<f12>") 'open-my-denote-directory)

;; ;; Consult-Notes for easy access to notes
;; (use-package! consult-notes
;;   :bind
;;   (("C-c n d F" . consult-notes)
;;    ("C-c n d g" . consult-notes-search-in-all-notes))
;;   :init
;;   (consult-notes-denote-mode))

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
          (writing . "You are a large language model and a writing assistant. Please write in Australian formal and academic English. The tone should be polished, clear, and professional but not overly verbose or unnecessarily complex. Avoid jargon unless essential to the topic, and ensure the writing maintains readability and precision.")
          (chat . "You are a large language model and a conversation partner. Respond concisely in Australian English.")
          (aussie . "G'day! You're a fair dinkum Aussie assistant living in Emacs. Respond in Australian English, using local slang and expressions where appropriate, mate."))))

(use-package! org-format
  :hook (org-mode . org-format-on-save-mode))

(use-package! oxr)

;; (after! org
;;   (setq org-todo-keywords
;;         '((sequence "LEER(l)" "|" "LEIDO(L)" "CANCELLED(c)"))))

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

(setq python-shell-interpreter "python3"
      python-shell-interpreter-args "-i")

(defun python-shell-send-paragraph-and-step ()
  "Send the current paragraph (block of code) to the Python REPL. If
no REPL is open, start one first. Move the cursor to the end of
the paragraph and step forward."
  (interactive)
  ;; Ensure the Python REPL is running
  (unless (comint-check-proc "*Python*")
    (run-python (python-shell-calculate-command) nil t))
  ;; Define the paragraph region
  (let ((start (save-excursion
                 (backward-paragraph)
                 (point)))
        (end (save-excursion
               (forward-paragraph)
               (point))))
    ;; Send the region to the REPL
    (python-shell-send-region start end))
  ;; Move the cursor to the end of the paragraph and step forward
  (forward-paragraph)
  ;; Ensure the cursor stays in the script buffer
  (pop-to-buffer (current-buffer)))


(defun python-shell-send-buffer-until-point ()
  "Send all lines from the beginning of the buffer up to the current
point to the Python REPL. If no REPL is open, start one first."
  (interactive)
  ;; Ensure the Python REPL is running
  (unless (comint-check-proc "*Python*")
    (run-python (python-shell-calculate-command) nil t))
  ;; Define the region from the beginning of the buffer to the current point
  (let ((start (point-min))
        (end (point)))
    (python-shell-send-region start end))
  ;; Optionally, focus the REPL
  ;; (pop-to-buffer "*Python*"))
  (pop-to-buffer (current-buffer)))


;; Add the keybinding after python-mode is loaded
(with-eval-after-load 'python
  (define-key python-mode-map (kbd "S-<return>") 'python-shell-send-paragraph-and-step)
  (define-key python-mode-map (kbd "C-c C-<up>") 'python-shell-send-buffer-until-point))

(setq org-agenda-prefix-format
      '((agenda . " %i %?-12t% s")
        (todo . " %i ")
        (tags . " %i ")
        (search . " %i ")))

;; ;; Auto commit for denote
;;    (use-package! git-auto-commit-mode
;;      :config
;;      (setq gac-default-message "Auto-commit"
;;            gac-debounce-interval 3600))  ;; Change to the desired interval in seconds

;; (after! org
;;   (setq org-latex-pdf-process
;;         '("latexmk -pdf -shell-escape -interaction=nonstopmode -output-directory=%o %f")))
(setq org-latex-pdf-process
      '("latexmk -pdflatex='pdflatex -shell-escape -interaction=nonstopmode' -pdf -output-directory=%o %f"))


(setq org-latex-listings 'minted)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Para que esto funcione                                      ;;
;; #+BIND: org-beamer-frame-default-options "allowframebreaks" ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq org-export-allow-bind-keywords t)
