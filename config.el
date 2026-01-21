;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!

;;; ========================================================================
;;; PERSONAL INFORMATION
;;; ========================================================================
(setq user-full-name "Alan Severini"
      user-mail-address "severini.alan@gmail.com")

;;; ========================================================================
;;; UI & APPEARANCE
;;; ========================================================================

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

;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
(setq display-line-numbers-type t)

(blink-cursor-mode 1)
(display-time-mode 1)
(display-battery-mode 1)
(setq global-hl-line-modes nil)

;; Disable overwrite-mode for ever!
(define-key global-map [(insert)] nil)

;;; ========================================================================
;;; GENERAL EDITING
;;; ========================================================================

(setq tab-always-indent 'complete)
;; Show completion UI in minibuffer (via consult), not popups


;; Performance optimizations
(setq gc-cons-threshold 100000000) ;; Increase garbage collection threshold
(setq read-process-output-max (* 1024 1024)) ;; Increase LSP performance

;; Para evitar que el tamaño de la fuente se vea reducida con superscripts o subscripts
(setq font-latex-fontify-script nil)

;; File associations
(add-to-list 'auto-mode-alist '("\\.apsim\\'" . xml-mode))
(add-to-list 'auto-mode-alist '("\\.apsimx\\'" . json-mode))
(add-to-list 'auto-mode-alist '("\\.slurm\\'" . bash-mode))
(add-to-list 'exec-path "/home/alancho/.cabal/bin")

;;; ========================================================================
;;; NAVIGATION & SEARCH
;;; ========================================================================

;; Probemos usar avy
(map! :leader
      (:prefix-map ("a" . "avy")
       :desc "Go to char timer" "t" #'avy-goto-char-timer))

;; Mejor manera de usar locate con consult
(setq consult-locate-args "locate --ignore-case --regex")

;; Custom ripgrep for org notes
(defun my-ripgrep-fixed-directory (&optional initial-input)
  (interactive)
  (consult-ripgrep "~/Dropbox/org" initial-input))

(map! :leader
      :desc "Ripgrep org notes"
      "s n" #'my-ripgrep-fixed-directory)

;;; ========================================================================
;;; PROJECTILE & COMPLETION
;;; ========================================================================

(setq projectile-track-known-projects-automatically t)
(setq projectile-sort-order 'recently-active)

(setq company-global-modes
      '(not ess-r-mode
        inferior-ess-r-mode
        python-mode
        emacs-lisp-mode
        eshell-mode
        markdown-mode
        org-mode
        shell-mode
        text-mode
        latex-mode))

(after! company
  (setq company-idle-delay 0.0
        company-minimum-prefix-length 1))

;; (use-package! company-box
;;   :hook (company-mode . company-box-mode))

;; Rebind yasnippet: disable TAB, use C-<tab> to expand
(after! yasnippet
  (define-key yas-minor-mode-map (kbd "<tab>") nil)
  (define-key yas-minor-mode-map (kbd "TAB") nil)
  ;; Keep snippets on C-<tab> but avoid Copilot clash
  (define-key yas-minor-mode-map (kbd "C-<tab>") nil)
  (define-key yas-minor-mode-map (kbd "C-c y") #'yas-expand))

;;; ========================================================================
;;; SPELL CHECKING
;;; ========================================================================

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
            ))

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

;;; ========================================================================
;;; KEYBINDINGS & ALIASES
;;; ========================================================================

;; (global-set-key (kbd "<f5>") #'polymode-toggle-chunk-narrowing)
(global-set-key (kbd "<f7>") #'unfill-toggle)
(global-set-key (kbd "<f8>") #'org-edit-special)

;; Aliases
(set-eshell-alias! "us" "sudo apt-get update && sudo apt-get upgrade && sudo apt-get clean"
                   "up" "uv sync"
                   "ll" "ls -lha"
                   "bunya" "ssh uqasever@bunya.rcc.uq.edu.au"
                   "tobunya" "rsync -avz --exclude '.git' --exclude '.*' $1 uqasever@bunya.rcc.uq.edu.au:/home/uqasever/$2"
                   "frombunya" "rsync -avz --include '$3' uqasever@bunya.rcc.uq.edu.au:/home/uqasever/$1 $2"
                   "ur" "r update-r-packages.R"
                   "un" "unison sandisco"
                   "ud" "doom sync && doom upgrade"
                   "ds" "dropbox status")

;;; ========================================================================
;;; WRITING & EDITING MODES
;;; ========================================================================

(add-hook 'markdown-mode-hook 'pandoc-mode)

(after! writeroom-mode
  (setq +zen-text-scale 1
        +zen-mixed-pitch-modes '(adoc-mode rst-mode)))

(after! figlet
  (setq figlet-default-font "banner"))

;; Olivetti toggle function
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

;;; ========================================================================
;;; ICONS & VISUAL ELEMENTS
;;; ========================================================================

;; Para que los icons de apsimx sean como los de json
(after! nerd-icons
  ;; Set the icon for 'apsimx' to use a JSON-like icon
  (add-to-list 'nerd-icons-extension-icon-alist
               '("apsimx" nerd-icons-codicon "nf-cod-settings" :face nerd-icons-yellow))
  (add-to-list 'nerd-icons-extension-icon-alist
               '("rds" nerd-icons-octicon "nf-oct-database" :face nerd-icons-orange))
  (add-to-list 'nerd-icons-extension-icon-alist
               '("slurm" nerd-icons-devicon "nf-dev-terminal" :face nerd-icons-purple))
  (add-to-list 'nerd-icons-extension-icon-alist
               '("tmpl" nerd-icons-devicon "nf-dev-terminal" :face nerd-icons-purple))
  (add-to-list 'nerd-icons-extension-icon-alist
               '("db" nerd-icons-octicon "nf-oct-database" :face nerd-icons-yellow)))

;;; ========================================================================
;;; CUSTOM UTILITY FUNCTIONS
;;; ========================================================================

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

;;; ========================================================================
;;; R/ESS CONFIGURATION
;;; ========================================================================

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
         ))

;;; ========================================================================
;;; ORG MODE CONFIGURATION
;;; ========================================================================

(setq org-directory "~/Dropbox/org")

(use-package! org
  :config
  (setq org-support-shift-select 'always
        org-return-follows-link t
        org-hide-emphasis-markers t
        org-level-color-stars-only nil
        org-replace-disputed-keys t
        org-image-actual-width '(500)
        org-src-window-setup 'current-window
        org-descriptive-links t
        org-refile-targets '((nil :maxlevel . 9) (org-agenda-files :maxlevel . 8))
        org-attach-store-link-p 'attached
        org-todo-keywords
        '((sequence "TODO(t)" "STRT(s)" "WAIT(w)" "HOLD(h)" "PROJ(p)" "|" "DONE(d)" "KILL(k)"))
        org-todo-keyword-faces
        '(("[-]" . +org-todo-active)
          ("STRT" . +org-todo-active)
          ("[?]" . +org-todo-onhold)
          ("WAIT" . +org-todo-onhold)
          ("HOLD" . +org-todo-onhold)
          ("PROJ" . +org-todo-project)
          ("NO" . +org-todo-cancel)
          ("KILL" . +org-todo-cancel))))

(after! org
  (set-popup-rule! "^\\*Org Src" :ignore t))

;; Prefiero que la fuente de los headings en org no sea bold
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(org-level-1 ((t (:inherit outline-1 :extend t :weight normal))))
 '(org-level-2 ((t (:inherit outline-2 :extend t :weight normal))))
 '(org-level-3 ((t (:inherit outline-3 :extend t :weight normal))))
 '(org-level-4 ((t (:inherit outline-4 :extend t :weight normal))))
 '(org-level-5 ((t (:inherit outline-5 :extend t :weight normal))))
 '(org-level-6 ((t (:inherit outline-6 :extend t :weight normal))))
 '(org-level-7 ((t (:inherit outline-7 :extend t :weight normal))))
 '(org-level-8 ((t (:inherit outline-8 :extend t :weight normal))))
 '(org-level-9 ((t (:inherit outline-9 :extend t :weight normal))))
 '(org-level-10 ((t (:inherit outline-10 :extend t :weight normal))))
 '(org-level-11 ((t (:inherit outline-11 :extend t :weight normal))))
 '(org-level-12 ((t (:inherit outline-12 :extend t :weight normal)))))


;; Add this line near the top of your config.el,
;; or at least before the my-org-project-tag function.
(defun my-org-project-todo-file ()
  "Return the path to the project's todo file, or a default if not in a project."
  (let* ((projectile-root (and (fboundp 'projectile-project-root)
                               (projectile-project-root)))
         (project-root (or projectile-root
                           (when (fboundp 'project-current)
                             (when-let ((project (project-current)))
                               (project-root project)))))
         (result (if project-root
                     (concat (file-name-as-directory project-root)
                             (file-name-nondirectory (directory-file-name project-root))
                             ".org")
                   (expand-file-name "~/Dropbox/org/todo.org"))))
    (message "Projectile root: %s, Project root: %s, Result: %s"
             projectile-root project-root result)
    result))

(defun my-org-project-tag ()
  "Return the project name as a tag for org capture."
  (let* ((file-path (my-org-project-todo-file))
         (filename (file-name-nondirectory file-path))
         (tag (file-name-sans-extension filename)))
    (replace-regexp-in-string "-" "_" tag)))

;; Org Agenda Configuration
(setq org-agenda-sorting-strategy
      '((agenda habit-down time-up urgency-down category-keep)
        (todo tag-down deadline-up priority-down timestamp-down)
        (tags urgency-down category-keep)
        (search category-keep)))

(setq org-agenda-prefix-format
      '((agenda  . " %i %-12:c%?-12t% s")
        (todo  . " %i %-30:c")
        (tags  . " %i %-30:c")
        (search . " %i %-12:c")))

(defun ads/agenda-restrict-this-project ()
  "Restrict agenda to current project"
  (interactive)
  (let ((org-agenda-files (list (projectile-project-root))))
    (org-agenda)))

(defun my/agenda-filter-by-file (filename)
  "Show agenda filtered to specific org file."
  (interactive "sFilename (e.g., project-alpha.org): ")
  (let ((org-agenda-files (list (expand-file-name filename))))
    (org-agenda nil "a")))

(defun ads/add-projectile-todo-files-to-agenda ()
  "Add all project org files from projectile directories to org-agenda-files."
  (let* ((projectile-cache-file "~/.config/emacs/.local/cache/projectile/projects.eld")
         (projectile-dirs (when (file-exists-p projectile-cache-file)
                            (with-temp-buffer
                              (insert-file-contents projectile-cache-file)
                              (read (current-buffer)))))
         (org-files (delq nil
                          (mapcar (lambda (dir)
                                    (let* ((project-name (file-name-nondirectory (directory-file-name dir)))
                                           (org-file (expand-file-name (concat project-name ".org") dir)))
                                      (when (file-exists-p org-file)
                                        org-file)))
                                  projectile-dirs))))
    org-files))

(setq org-agenda-files
      (append '("/home/alancho/Dropbox/org/inbox.org"
                "/home/alancho/Dropbox/org/notes.org"
                "/home/alancho/Dropbox/org/projects.org"
                "/home/alancho/Dropbox/org/todo.org")
              (ads/add-projectile-todo-files-to-agenda)))

(use-package! oxr)

;; Para evitar que el tamaño de la fuente se vea reducida con superscripts o subscripts
(setq font-latex-fontify-script nil)

;; (global-set-key (kbd "<f5>") #'polymode-toggle-chunk-narrowing)
(global-set-key (kbd "<f7>") #'unfill-toggle)
(global-set-key (kbd "<f8>") #'org-edit-special)

;; Aliases
(set-eshell-alias! "us" "sudo apt-get update && sudo apt-get upgrade && sudo apt-get clean"
                   "up" "uv sync"
                   "ll" "ls -lha"
                   "bunya" "ssh uqasever@bunya.rcc.uq.edu.au"
                   "tobunya" "rsync -avz --exclude '.git' --exclude '.*' $1 uqasever@bunya.rcc.uq.edu.au:/home/uqasever/$2"
                   "frombunya" "rsync -avz --include '$3' uqasever@bunya.rcc.uq.edu.au:/home/uqasever/$1 $2"
                   "ur" "r update-r-packages.R"
                   "un" "unison sandisco"
                   "ud" "doom sync && doom upgrade"
                   "ds" "dropbox status")
;;; ========================================================================
;;; NOTES & BIBLIOGRAPHY
;;; ========================================================================

;; biblio
(setq! citar-bibliography '("~/Dropbox/Papers/library.bib")
       org-cite-global-bibliography '("~/Dropbox/Papers/library.bib")
       citar-library-paths '("~/Dropbox/Papers/")
       citar-notes-paths '("~/Dropbox/notes/"))

(map! :map doom-leader-notes-map
      "b" #'citar-insert-citation)

;;; ========================================================================
;;; AI/LLM CONFIGURATION
;;; ========================================================================

(use-package! gptel
  :config
  (setq gptel-default-mode 'org-mode
        gptel-model 'google/gemini-2.0-flash-001)
  ;; OpenRouter (default)
  (setq gptel-backend
        (gptel-make-openai "OpenRouter"
          :host "openrouter.ai"
          :endpoint "/api/v1/chat/completions"
          :stream t
          :key (lambda ()
                 (or (getenv "OPENROUTER_API_KEY")
                     (auth-source-pick-first-password :host "openrouter.ai")
                     (user-error "Set OPENROUTER_API_KEY or auth-source for openrouter.ai")))
          :models '("google/gemini-2.0-flash-001"
                    "anthropic/claude-sonnet-4.5"
                    "openai/gpt-5"
                    "google/gemini-2.5-flash")))
  ;; OpenAI
  (gptel-make-openai "ChatGPT"
    :host "api.openai.com"
    :endpoint "/v1/chat/completions"
    :stream t
    :key (lambda ()
           (or (getenv "OPENAI_API_KEY")
               (auth-source-pick-first-password :host "api.openai.com")
               (user-error "Set OPENAI_API_KEY or auth-source for api.openai.com")))
    :models '("gpt-5" "gpt-4o" "gpt-4o-mini" "o4-mini"))
  ;; Anthropic
  (gptel-make-anthropic "Claude"
    :stream t
    :key (lambda ()
           (or (getenv "ANTHROPIC_API_KEY")
               (auth-source-pick-first-password :host "api.anthropic.com")
               (user-error "Set ANTHROPIC_API_KEY or auth-source for api.anthropic.com")))
    :models '(claude-3-7-sonnet-20250219 claude-3-5-haiku-20241022 claude-3-opus-20240229))
  ;; Gemini
  (gptel-make-gemini "Gemini"
    :stream t
    :key (lambda ()
           (or (getenv "GEMINI_API_KEY")
               (auth-source-pick-first-password :host "generativelanguage.googleapis.com")
               (user-error "Set GEMINI_API_KEY or auth-source for generativelanguage.googleapis.com")))))

;;; ========================================================================
;;; PYTHON DEVELOPMENT
;;; ========================================================================

(defun ads/python-compile-buffer-with-uv ()
  "Save current buffer and run it with uv (if available) in a compile buffer."
  (interactive)
  (unless (buffer-file-name)
    (user-error "Buffer is not visiting a file"))
  (save-buffer)
  (let* ((file (buffer-file-name))
         (proj (or (and (fboundp 'projectile-project-root)
                        (ignore-errors (projectile-project-root)))
                   (when (fboundp 'project-current)
                     (when-let ((proj (project-current)))
                       (car (project-roots proj))))
                   default-directory))
         (default-directory (or proj default-directory))
         (cmd (if (executable-find "uv")
                  (format "uv run python %s" (shell-quote-argument file))
                (format "python %s" (shell-quote-argument file)))))
    (compile cmd)))

(defun python-shell-send-paragraph (&optional send-main msg)
  "Send blocks between two white lines to inferior Python process.
See `python-shell-send-region' for SEND-MAIN and MSG.
Opens REPL on first call if not running, sends code on subsequent calls."
  (interactive "P\ni")
  (let ((current-buffer (current-buffer)))
    (if (python-shell-get-process)
        ;; Process is running, send code
        (let ((start
               (save-excursion
                 (if (re-search-backward "^[:blank:]*$" nil t)
                     (let ((pos (point)))
                       (python-nav-end-of-block)
                       (if (< (point) pos) pos
                         (progn (python-nav-beginning-of-block) (point))))
                   (point-min))))
              (end
               (save-excursion
                 (if (re-search-forward  "^[:blank:]*$" nil t)
                     (progn (python-nav-end-of-block) (point))
                   (point-max)))))
          (python-shell-send-region start end send-main (not msg) nil)
          ;; Move to next paragraph after sending, skipping blank lines and comments
          (goto-char end)
          (when (re-search-forward "^[:blank:]*$" nil t)
            (forward-line 1)
            (while (and (not (eobp))
                        (or (looking-at "^[:blank:]*$")
                            (looking-at "^[:blank:]*#")))
              (forward-line 1))))
      ;; Process not running, start it and return to script buffer
      (run-python nil nil t)
      (switch-to-buffer-other-window current-buffer))))

(defun python-shell-send-to-current-line (&optional send-main msg)
  "Send code from beginning of buffer to current line to inferior Python process.
See `python-shell-send-region' for SEND-MAIN and MSG.
Opens REPL on first call if not running, sends code on subsequent calls."
  (interactive "P\ni")
  (let ((current-buffer (current-buffer)))
    (if (python-shell-get-process)
        ;; Process is running, send code
        (let ((start (point-min))
              (end (save-excursion
                     (end-of-line)
                     (point))))
          (python-shell-send-region start end send-main (not msg) nil)
          ;; Move to next line after sending, skipping blank lines and comments
          (forward-line 1)
          (while (and (not (eobp))
                      (or (looking-at "^[:blank:]*$")
                          (looking-at "^[:blank:]*#")))
            (forward-line 1)))
      ;; Process not running, start it and return to script buffer
      (run-python nil nil t)
      (switch-to-buffer-other-window current-buffer))))

(with-eval-after-load 'python
  ;; Use IPython as the Python shell interpreter
  ;; (setq python-shell-interpreter "ipython"
  ;;       python-shell-interpreter-args "-i --simple-prompt")
  (setq python-shell-interpreter "jupyter"
        python-shell-interpreter-args "console --simple-prompt"
        ;; Disable the warning as it is often a false positive with Jupyter
        python-shell-prompt-detect-failure-warning nil)
  ;; Match the standard Jupyter/IPython "In [1]:" and "Out[1]:" prompts
  (setq python-shell-prompt-regexp "In \\[[0-9]+\\]: "
        python-shell-prompt-output-regexp "Out\\[[0-9]+\\]: ")
  ;; Native completion can sometimes conflict with jupyter console
  (setq python-shell-completion-native-enable nil)
  ;; (define-key python-mode-map (kbd "S-<return>") #'ads/python-compile-buffer-with-uv)
  (define-key python-mode-map (kbd "S-<return>") #'python-shell-send-paragraph)
  (define-key python-mode-map (kbd "C-c C-<up>") #'python-shell-send-to-current-line)
  ;; Ensure TAB is reserved for indent/completion-in-minibuffer
  (define-key python-mode-map (kbd "TAB") #'indent-for-tab-command)
  (define-key python-mode-map (kbd "<tab>") #'indent-for-tab-command))

(after! python
  (set-eglot-client! '(python-mode python-ts-mode) '("ty" "server")))

;;; ========================================================================
;;; LATEX CONFIGURATION
;;; ========================================================================

;; (setq org-latex-pdf-process
;;       '("latexmk -pdflatex='pdflatex -shell-escape -interaction=nonstopmode' -pdf -output-directory=%o %f"))

(setq org-latex-listings 'minted)

;; Para que esto funcione: #+BIND: org-beamer-frame-default-options "allowframebreaks"
(setq org-export-allow-bind-keywords t)

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
  (let ((output-dir
         (if (and buffer-file-name
                  (string-match-p (expand-file-name "~/Dropbox/denotes/") buffer-file-name))
             "~/Downloads"
           pub-dir)))
    (funcall orig-fun extension subtreep output-dir)))

(advice-add 'org-export-output-file-name :around #'my-org-export-output-file-name)

(defun ess-render-current-file ()
  "Execute rmarkdown::render() on the current R script file."
  (interactive)
  (when (not (derived-mode-p 'ess-r-mode))
    (user-error "Not in an R buffer"))

  (let ((file-name (buffer-file-name)))
    (when (not file-name)
      (user-error "Buffer is not visiting a file"))

    (save-buffer)
    (ess-force-buffer-current "R process to use: ")
    (let ((command (format "rmarkdown::render(\"%s\")"
                           (replace-regexp-in-string "\\\\" "\\\\\\\\" file-name))))
      (ess-eval-linewise command))))

(defun create-slurm-r-script ()
  "Create a Slurm script for the current R file with predefined scenarios."
  (interactive)
  (let* ((current-file (buffer-file-name))
         (current-dir (file-name-directory current-file))
         (r-script-name (file-name-nondirectory current-file))
         (base-name (file-name-sans-extension r-script-name)))

    ;; Check if current buffer is an R file
    (unless (and current-file (string-match "\\.R$" current-file))
      (error "Current buffer is not an R file"))

    ;; Select scenario
    (let ((scenario (completing-read "Select scenario: "
                                     '(("master" . "Minimal resources, single job")
                                       ("array" . "Array job with 1 CPU each")
                                       ("compute" . "High-resource single job"))
                                     nil t))
          (slurm-script-name nil)
          (slurm-script-path nil))

      ;; Set script name based on scenario
      (setq slurm-script-name (concat scenario ".slurm"))
      (setq slurm-script-path (concat current-dir slurm-script-name))

      (cond
       ;; MASTER scenario: minimal resources, time as only argument
       ((string= scenario "master")
        (let* ((time-limit (read-string "Time limit (HH:MM:SS): " "00:30:00"))
               (job-name (read-string "Job name: " (concat base-name "-master")))
               (partition (read-string "Partition: " "general"))
               (modules (read-string "Modules (space-separated): " "r nlopt gcc/12.3.0"))
               (qos (read-string "QOS: " "normal"))
               (output-file (concat base-name "_%j.out"))
               (error-file (concat base-name "_%j.err")))

          (create-slurm-script-content slurm-script-path job-name output-file error-file
                                       time-limit partition qos nil "1" "1G" modules
                                       r-script-name time-limit nil nil)))

       ;; ARRAY scenario: array jobs, array ID as only argument
       ((string= scenario "array")
        (let* ((array-range (read-string "Array range: " "1-1000"))
               (time-limit (read-string "Time limit per task (HH:MM:SS): " "01:00:00"))
               (job-name (read-string "Job name: " (concat base-name "-array")))
               (partition (read-string "Partition: " "general"))
               (modules (read-string "Modules (space-separated): " "r nlopt gcc/12.3.0"))
               (qos (read-string "QOS: " "normal"))
               (output-file (concat base-name "_%A_%a.out"))
               (error-file (concat base-name "_%A_%a.err")))

          (create-slurm-script-content slurm-script-path job-name output-file error-file
                                       time-limit partition qos array-range "1" "2G" modules
                                       r-script-name nil array-range nil)))

       ;; COMPUTE scenario: high resources, custom arguments
       ((string= scenario "compute")
        (let* ((time-limit (read-string "Time limit (HH:MM:SS): " "04:00:00"))
               (job-name (read-string "Job name: " (concat base-name "-compute")))
               (partition (read-string "Partition: " "general"))
               (modules (read-string "Modules (space-separated): " "r nlopt gcc/12.3.0"))
               (qos (read-string "QOS: " "normal"))
               (memory (read-string "Memory (e.g., 4G, 16G, 32G): " "16G"))
               (cpus-per-task (read-string "CPUs per task: " "4"))
               (output-file (concat base-name "_%j.out"))
               (error-file (concat base-name "_%j.err")))

          (create-slurm-script-content slurm-script-path job-name output-file error-file
                                       time-limit partition qos nil cpus-per-task memory modules
                                       r-script-name nil nil cpus-per-task))))

      (message "Created %s Slurm script: %s" scenario slurm-script-name))))

(defun create-slurm-script-content (script-path job-name output-file error-file
                                                time-limit partition qos array-range cpus-per-task
                                                memory modules r-script-name time-arg array-arg cpu-arg)
  "Helper function to create the actual Slurm script content."
  ;; Generate module load commands
  (let ((module-lines (if (or (not modules) (string-empty-p modules))
                          "# No modules specified"
                        (mapconcat (lambda (module)
                                     (format "module load %s" module))
                                   (split-string modules)
                                   "\n")))
        ;; Build R script arguments
        (r-args (concat
                 (if time-arg time-arg "")
                 (if array-arg " $SLURM_ARRAY_TASK_ID" "")
                 (if cpu-arg " $SLURM_CPUS_PER_TASK" ""))))

    ;; Create Slurm script content
    (let ((slurm-content
           (format "#!/bin/bash
#SBATCH --job-name=%s
#SBATCH --time=%s
#SBATCH --partition=%s
#SBATCH --qos=%s%s
#SBATCH --ntasks=1
#SBATCH --cpus-per-task=%s
#SBATCH --mem=%s
#SBATCH --account=a_qaafi_ccs

# Load modules
%s

# Print job information
echo \"Job started at: $(date)\"
echo \"Running on node: $(hostname)\"
echo \"Job ID: $SLURM_JOB_ID\"%s
echo \"Time limit: %s\"
echo \"QOS: %s\"
echo \"CPUs per task: %s\"
echo \"Memory: %s\"

# Run R script with arguments
Rscript %s%s

echo \"Job finished at: $(date)\"
"
                   job-name
                   time-limit
                   partition
                   qos
                   (if array-range (format "\n#SBATCH --array=%s" array-range) "")
                   cpus-per-task
                   memory
                   module-lines
                   (if array-range "\necho \"Array task ID: $SLURM_ARRAY_TASK_ID\"" "")
                   time-limit
                   qos
                   cpus-per-task
                   memory
                   r-script-name
                   (if (string-empty-p r-args) "" (concat " " r-args)))))

      ;; Write Slurm script to file
      (with-temp-file script-path
        (insert slurm-content))

      ;; Open the created Slurm script
      (find-file script-path))))

(setq org-babel-R-command "R --no-init-file --quiet --slave --no-save")
(setq org-babel-default-header-args:R
      '((:session . "none")
        (:prologue . "suppressPackageStartupMessages(require(tidyverse)); suppressPackageStartupMessages(require(knitr)); source('~/Dropbox/R/theme_alan.R')")))

;; (use-package! copilot
;;   :hook (;;(python-mode . copilot-mode)
;;          ;; (ess-mode . copilot-mode)
;;          )
;;   :init
;;   (setq copilot-idle-delay 0.4)
;;   :bind (:map copilot-completion-map
;;               ("C-<tab>" . 'copilot-accept-completion)
;;               ("C-S-<tab>" . 'copilot-accept-completion-by-word)))

(defun my/create-project ()
  "Create a new R or Python project with sensible defaults and live uv output."
  (interactive)
  (let* ((project-type (completing-read "Project type: " '("R" "Python") nil t))
         (project-name (read-string "Project name: "))
         (project-dir (read-directory-name "Create in directory: "))
         (deps (if (string= project-type "Python")
                   (read-string "Initial Python dependencies (comma or space separated, empty for none): ")
                 ""))  ;; Skip prompt for R
         (git-init (y-or-n-p "Initialize a Git repository? "))
         (default-directory (expand-file-name project-dir))
         (project-path (expand-file-name project-name project-dir))
         (envrc-path (expand-file-name ".envrc" project-path))
         (subdirs '("data" "functions" "outputs" "slides"))
         (presentation-src "~/Dropbox/templates/org/beamer.org")
         (presentation-dest (expand-file-name "slides/presentation.org" project-path))
         (rscript-path (expand-file-name "010.R" project-path))
         (gitignore-path (expand-file-name ".gitignore" project-path))
         (deps-list (split-string deps "[ ,]+" t)))

    ;; Create base directory
    (unless (file-directory-p project-path)
      (make-directory project-path t))

    ;; Create common subdirectories
    (dolist (sub subdirs)
      (make-directory (expand-file-name sub project-path) t))

    ;; Copy presentation template if it exists
    (when (file-exists-p presentation-src)
      (copy-file presentation-src presentation-dest t))

    ;; Create .gitignore
    (unless (file-exists-p gitignore-path)
      (with-temp-file gitignore-path
        (insert (mapconcat #'identity
                           '("*"
                             "!*/"
                             "!.gitignore"
                             "!*.R"
                             "!*.r"
                             "!*.Rmd"
                             "!*.py"
                             "!*.csv"
                             "!*.tsv"
                             "!*.txt"
                             "!*.md"
                             "!*.org"
                             "!*.json"
                             "!*.yml"
                             "!*.yaml"
                             "!*.toml"
                             "!*.ini"
                             "!*.cfg"
                             "!*.conf"
                             "!*.tex"
                             "!*.bib"
                             "!*.sh"
                             "!*.bash"
                             "!*.zsh"
                             "!*.qmd")
                           "\n"))))

    ;; Branch by project type
    (pcase project-type
      ("R"
       (message "🧬 Setting up R project...")
       (unless (file-exists-p rscript-path)
         (with-temp-file rscript-path
           (insert "require(tidyverse)\n"))))

      ("Python"
       (message "🐍 Setting up Python project with uv...")
       (let ((default-directory project-path))
         ;; Initialize uv project
         (unless (zerop (call-process "uv" nil "*uv-init*" t "init"))
           (error "❌ Failed to run 'uv init' — check if uv is installed"))

         ;; Create .envrc for direnv
         (with-temp-file envrc-path
           (if (file-exists-p "~/.config/direnv/lib/use_uv.sh")
               (insert "use uv\n")
             (insert "if [ -d .venv ]; then\n  source .venv/bin/activate\nelse\n  uv sync && source .venv/bin/activate\nfi\n")))

         ;; Allow direnv
         (call-process "direnv" nil "*direnv-allow*" t "allow")

         ;; Install dependencies with live output
         (when deps-list
           (when (get-buffer "*uv-install*")
             (kill-buffer "*uv-install*"))
           (let ((uv-buffer (get-buffer-create "*uv-install*")))
             (with-current-buffer uv-buffer
               (erase-buffer)
               (insert (format "📦 Installing Python dependencies in %s:\n\n" project-name)))
             (display-buffer uv-buffer)
             (dolist (pkg deps-list)
               (with-current-buffer uv-buffer
                 (insert (format "→ Running: uv add %s\n\n" pkg)))
               (let ((exit-code (call-process "uv" nil uv-buffer t "add" pkg)))
                 (if (zerop exit-code)
                     (with-current-buffer uv-buffer
                       (insert (format "\n✅ Successfully added %s\n\n" pkg)))
                   (with-current-buffer uv-buffer
                     (insert (format "\n⚠️ Failed to add %s (exit code %s)\n\n"
                                     pkg exit-code))))))
             (with-current-buffer uv-buffer
               (goto-char (point-max)))
             (message "✅ Dependency installation complete — see *uv-install* buffer for details."))))))

    ;; Initialize Git if requested
    (when git-init
      (message "🔧 Initializing Git repository...")
      (let ((default-directory project-path))
        (call-process "git" nil "*git-init*" t "init")
        (call-process "git" nil "*git-add*" t "add" ".")
        (call-process "git" nil "*git-commit*" t "-m" "Initial commit")))

    ;; Register with Projectile
    (when (featurep 'projectile)
      (projectile-add-known-project project-path))

    ;; Open in Dired
    (dired project-path)

    (message "🎉 %s project '%s' ready at %s%s%s"
             project-type
             project-name
             project-path
             (if (and (string= project-type "Python") deps-list)
                 (format " with deps: [%s]" (string-join deps-list ", "))
               "")
             (if git-init " (Git initialized)" ""))))

(defun my/format-for-logseq (beg end)
  "Convert Markdown headers and paragraphs to Logseq-style nested bullets.
Processes the active region or the entire buffer if no region is active."
  (interactive "r")
  ;; If no region is active, use the whole buffer
  (unless (use-region-p)
    (setq beg (point-min)
          end (point-max)))
  (let ((input (buffer-substring-no-properties beg end)))
    (with-temp-buffer
      (insert input)
      ;; 1. Convert headers (# Header) to bullets with headers (- # Header)
      (goto-char (point-min))
      (while (re-search-forward "^\\(#+ .+\\)$" nil t)
        (replace-match "- \\1"))

      ;; 2. Convert plain paragraphs to indented bullets
      ;; This looks for lines that don't start with - or # or whitespace
      (goto-char (point-min))
      (while (re-search-forward "^\\([^-\s#\n].+\\)$" nil t)
        (replace-match "  - \\1"))

      ;; 3. Indent existing lists so they become children
      (goto-char (point-min))
      (while (re-search-forward "^\\([-*]\\|\s+[0-9]+\\.\\) " nil t)
        (unless (save-excursion (backward-char 2) (looking-at-p "- "))
          (replace-match "  - ")))

      ;; 4. Clean up: Remove triple newlines that cause empty blocks in Logseq
      (goto-char (point-min))
      (while (re-search-forward "\n\n\n+" nil t)
        (replace-match "\n\n"))

      ;; Copy result to clipboard and notify user
      (kill-new (buffer-string))
      (message "Text formatted and copied to clipboard for Logseq!"))))
