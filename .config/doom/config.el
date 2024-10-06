;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-


;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!


;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets. It is optional.
(setq user-full-name "Mumtahin Farabi"
      user-mail-address "mfarabi619@gmail.com")

;; Doom exposes five (optional) variables for controlling fonts in Doom:
;;
;; - `doom-font' -- the primary font to use
;; - `doom-variable-pitch-font' -- a non-monospace font (where applicable)
;; - `doom-big-font' -- used for `doom-big-font-mode'; use this for
;;   presentations or streaming.
;; - `doom-symbol-font' -- for symbols
;; - `doom-serif-font' -- for the `fixed-pitch-serif' face
;;
;; See 'C-h v doom-font' for documentation and more examples of what they
;; accept. For example:
;;
;; (setq doom-font (font-spec :family "Fira Code" :size 12 :weight 'semi-light))
;; doom-variable-pitch-font (font-spec :family "Fira Sans" :size 13))
;;
;; If you or Emacs can't find your font, use 'M-x describe-font' to look them
;; up, `M-x eval-region' to execute elisp code, and 'M-x doom/reload-font' to
;; refresh your font settings. If Emacs still can't find your font, it likely
;; wasn't installed correctly. Font issues are rarely Doom issues!

;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. This is the default:
;; (setq doom-theme 'doom-gruvbox)
(setq doom-theme 'doom-lantern)
(setq fancy-splash-image "~/dotfiles/.config/doom/doom-emacs-color2.svg")

;; Custom banner with a quote below it in Doom Emacs dashboard
(after! doom-dashboard
  ;; Function to set a banner image (replace with your banner setup)
  (setq +doom-dashboard-banner-dir "~/.doom.d/banners/")
  (setq +doom-dashboard-banner-file "your-image-file.png") ;; Set your banner file path here

  ;; Custom text to display after the banner
  (add-to-list '+doom-dashboard-functions
               (lambda ()
                 (insert "\n"
                         "Do not proceed with a mess; messes just grow with time.\n"
                         "― Bjarne Stroustrup\n\n"))))


;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
(setq display-line-numbers-type t)

;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
(setq org-directory "~/org/")


;; Whenever you reconfigure a package, make sure to wrap your config in an
;; `after!' block, otherwise Doom's defaults may override your settings. E.g.
;;
;;   (after! PACKAGE
;;     (setq x y))
;;
;; The exceptions to this rule:
;;
;;   - Setting file/directory variables (like `org-directory')
;;   - Setting variables which explicitly tell you to set them before their
;;     package is loaded (see 'C-h v VARIABLE' to look up their documentation).
;;   - Setting doom variables (which start with 'doom-' or '+').
;;
;; Here are some additional functions/macros that will help you configure Doom.
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
;; Alternatively, use `C-h o' to look up a symbol (functions, variables, faces,
;; etc).
;;
;; You can also try 'gd' (or 'C-c c d') to jump to their definition and see how
;; they are implemented.

(after! treemacs
  ;; Open treemacs on the right side instead of the left
  (setq treemacs-position 'right)

  ;; Configure Treemacs Theme
  (treemacs-load-theme "nerd-icons")
  ;; (treemacs-load-theme "Default")
  )

;; Set default browser
(setq browse-url-browser-function 'browse-url-default-browser)

;; Set default search engine to Google
(setq engine/search-engine 'google)

;; Tell Projectile to search these directories
(setq projectile-project-search-path '("~/workspace/" "~/Documents/"))

;; Set LaTeX PDF viewer
;; (setq +latex-viewers '(texlive-lualatex))

;; Move by visual lines instead of physical lines
(define-key evil-normal-state-map (kbd "j") 'evil-next-visual-line)
(define-key evil-normal-state-map (kbd "k") 'evil-previous-visual-line)

;; Display relative line numbers
(menu-bar--display-line-numbers-mode-relative)

;; Turn on LSP mode
;; (lsp-mode)

;; Enable lsp-mode in web-mode
(add-hook 'web-mode-hook #'lsp)
(add-hook 'web-mode-hook (lambda () (setq +format-with-lsp t)))

;; Set directory for org mode
(setq org-directory
      "~/Documents/Obsidian Vault/misc")

(after! org
  ;; Allow linking to non-headlines in org mode
  (setq org-link-search-must-match-exact-headline nil)

  ;; Set custom todo keywords
  (setq org-todo-keywords
        '((sequence "TODO(t)" "INPROGRESS(i)" "BLOCKED(b)" "|" "DONE(d)" "CANCELLED(c)")))

  ;; Set custom colors for todo keywords
  (setq org-todo-keyword-faces
        '(("TODO" :foreground "#7c7c75" :weight normal :underline t)
          ("INPROGRESS" :foreground "#0098dd" :weight normal :underline t)
          ("DONE" :foreground "#50a14f" :weight normal :underline t)
          ("CANCELLED" :foreground "#ff6480" :weight normal :underline t)
          ("BLOCKED" :foreground "#ff9800" :weight normal :underline t)))

  ;; Set custom colors for priorities
  (setq org-priority-faces
        '((?A :foreground "#e45649")
          (?B :foreground "#da8548")
          (?C :foreground "#0098dd")))

  )

;; Add icons to priorities
(use-package! org-fancy-priorities
  :hook (org-mode . org-fancy-priorities-mode)
  :config
  (setq org-fancy-priorities-list '("■" "■" "■"))
  )

;; Better alternatives for M-x, from 'Effective Emacs' by Steve Yegge: https://sites.google.com/site/steveyegge2/effective-emacs
(global-set-key "\C-x\C-m" 'execute-extended-command)
(global-set-key "\C-c\C-m" 'execute-extended-command)

;; Detect package manager for web dev project
(defun my/detect-package-manager ()
  "Detect if the project is using pnpm, yarn, or npm."
  (cond
   ((file-exists-p (expand-file-name "pnpm-lock.yaml" (projectile-project-root))) "pnpm")
   ((file-exists-p (expand-file-name "yarn.lock" (projectile-project-root))) "yarn")
   ((file-exists-p (expand-file-name "package-lock.json" (projectile-project-root))) "npm")
   (t "npm"))) ;; Default to npm if none is found

(defun my/package-manager-run (script)
  "Run a script from package.json using the detected package manager."
  (interactive
   (list (completing-read
          "Run script: "
          (npm-mode--available-scripts))))
  (let* ((package-manager (my/detect-package-manager))
         (command (concat package-manager " run " script)))
    (compile command)))
