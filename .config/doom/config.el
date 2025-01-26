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
(setq doom-theme 'doom-lantern
      doom-lantern-padded-modeline t)

(setq doom-font
      (font-spec :family "JetBrainsMono Nerd Font"
                 :size 20)
      doom-variable-pitch-font
      (font-spec :family "JetBrainsMono Nerd Font"
                 :size 20)
      doom-big-font
      (font-spec :family "JetBrainsMono Nerd Font"
                 :size 40))

;; Custom banner with a quote below it in Doom Emacs dashboard
(setq fancy-splash-image "~/dotfiles/.config/doom/doom-emacs-color2.svg")
;; Custom text to display after the banner
(after! doom-dashboard
  (defun my-custom-dashboard-text ()
    "Insert custom text into the Doom dashboard."
    (insert "\"Do not proceed with a mess; messes just grow with time.\" ― Bjarne Stroustrup\n\n"))

  ;; Find `doom-dashboard-widget-banner` in the list and insert after it
  (let ((pos (cl-position #'doom-dashboard-widget-banner +doom-dashboard-functions)))
    (when pos
      (setq +doom-dashboard-functions
            (append (cl-subseq +doom-dashboard-functions 0 (1+ pos))
                    (list #'my-custom-dashboard-text)
                    (cl-subseq +doom-dashboard-functions (1+ pos)))))))

;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
(setq org-directory "~/Documents/")

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
;; - `add-load-path!rectories to the `load-path', relative to
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

;; https://tecosaur.github.io/emacs-config

;; :which-key
;; Make popup faster
(setq which-key-idle-delay 0.25)
;; Remove 'evil-' in too many popups
(setq which-key-allow-multiple-replacements t)
(after! which-key
  (pushnew!
   which-key-replacement-alist
   '(("" . "\\`+?evil[-:]?\\(?:a-\\)?\\(.*\\)") . (nil . "◂\\1"))
   '(("\\`g s" . "\\`evilem--?motion-\\(.*\\)") . (nil . "◃\\1"))
   ))

;; LLM Integration
(use-package! gptel
  :commands gptel gptel-menu gptel-mode gptel-send gptel-set-tpic
  :config
  (let (ollama-models)
    (when (executable-find "ollama")
      (with-temp-buffer
        (call-process "ollama" nil t nil "list")
        (goto-char (point-min))
        (forward-line 1)
        (while (and (not (eobp)) (looking-at "[^ \t]+"))
          (push (match-string 0) ollama-models)
          (forward-line 1))))
    (setq-default gptel-model "nous-hermes2:latest"
                  gptel-backend (gptel-make-ollama "Ollama" :models ollama-models :stream t)))
  (setq gptel-default-mode #'org-mode))

(display-time-mode 1)
(setq display-time-day-and-date t)

;; Set default browser
(setq browse-url-browser-function 'browse-url-default-browser)

;; Set default search engine to Google
(setq engine/search-engine 'google)

;; Tell Projectile to search these directories
(setq projectile-project-search-path '("~/workspace/" "~/Documents/"))

;;; :editor evil
;; Focus new window after splitting
(setq evil-split-window-below t
      evil-vsplit-window-right t)
(defadvice! prompt-for-buffer (&rest _)
  :after '(evil-window-split evil-window-vsplit)
  (consult-buffer))
(map! :map evil-window-map
      "SPC" #'rotate-layout
      ;; Navigation
      "<left>"     #'evil-window-left
      "<down>"     #'evil-window-down
      "<up>"       #'evil-window-up
      "<right>"    #'evil-window-right
      ;; Swapping windows
      "C-<left>"       #'+evil/window-move-left
      "C-<down>"       #'+evil/window-move-down
      "C-<up>"         #'+evil/window-move-up
      "C-<right>"      #'+evil/window-move-right)

;; Implicit /g flag on evil ex substitution, because I use the default behavior less often.
(setq evil-ex-substitute-global t)

;; Move by visual lines instead of physical lines
(define-key evil-normal-state-map (kbd "j") 'evil-next-visual-line)
(define-key evil-normal-state-map (kbd "k") 'evil-previous-visual-line)

;; (keycast-tab-line-mode)

(setq evil-escape-key-sequence "jk")

;; Make flycheck errors much better
(set-popup-rule! "^\\*Flycheck errors\\*$" :side 'bottom :size 0.4 :select t)

;; Set directory for org mode
(setq org-directory
      "~/Documents/Obsidian Vault/misc")

(after! org
  ;; Allow linking to non-headlines in org mode
  (setq org-link-search-must-match-exact-headline nil)

  ;; Set custom todo keywords
  (setq org-todo-keywords '((sequence "TODO(t)" "INPROGRESS(i)" "BLOCKED(b)" "|" "DONE(d)" "CANCELLED(c)")))

  ;; Set custom colors for todo keywords
  (setq org-todo-keyword-faces '(("TODO" :foreground "#7c7c75" :weight normal :underline t)
                                 ("INPROGRESS" :foreground "#0098dd" :weight normal :underline t)
                                 ("DONE" :foreground "#50a14f" :weight normal :underline t)
                                 ("CANCELLED" :foreground "#ff6480" :weight normal :underline t)
                                 ("BLOCKED" :foreground "#ff9800" :weight normal :underline t)))

  ;; Set custom colors for priorities
  (setq org-priority-faces
        '((?A :foreground "#e45649")
          (?B :foreground "#da8548")
          (?C :foreground "#0098dd"))))

;; Add icons to priorities
;; (use-package! org-fancy-priorities
;;   :hook (org-mode . org-fancy-priorities-mode)
;;   :config
;;   (setq org-fancy-priorities-list '("■" "■" "■"))
;;   )

(after! org
  (define-key org-mode-map (kbd "C-c C-r") verb-command-map))

;; magit-todos
(after! magit
  (setq magit-diff-refine-hunk 'all)

  (use-package! magit-todos
    :config (magit-todos-mode 1))

  (use-package! magit-file-icons
    :config (magit-file-icons-mode 1)
    ))

;; TODO: Read Better alternatives for M-x, from 'Effective Emacs' by Steve Yegge: https://sites.google.com/site/steveyegge2/effective-emacs

;; When viewing a pdf, view it in dark mode instead of the default light mode
(add-hook
 'pdf-view-mode-hook
 'pdf-view-midnight-minor-mode)

(use-package! org-pandoc-import :after org)

;; Remap switching to last buffer from 'SPC+`' to 'SPC+e'
(map! :leader
      :desc "Switch to last buffer" "e"
      #'evil-switch-to-windows-last-buffer)

;; Display relative line numbers
(menu-bar--display-line-numbers-mode-relative)
(setq display-line-numbers-type t)

(nyan-mode)

(after! modeline
  :config
  (setq doom-modeline-height 45)
  (setq doom-modeline-persp-name t))

;; https://www.ovistoica.com/blog/2024-7-05-modern-emacs-typescript-web-tsx-config

;;     ;; Optional, but recommended. Tree-sitter enabled major modes are
;;     ;; distinct from their ordinary counterparts.
;;     ;;
;;     ;; You can remap major modes with `major-mode-remap-alist'. Note
;;     ;; that this does *not* extend to hooks! Make sure you migrate them
;;     ;; also
;;     (dolist (mapping
;;              '((python-mode . python-ts-mode)
;;                (css-mode . css-ts-mode)
;;                (typescript-mode . typescript-ts-mode)
;;                (js-mode . typescript-ts-mode)
;;                (js2-mode . typescript-ts-mode)
;;                (c-mode . c-ts-mode)
;;                (c++-mode . c++-ts-mode)
;;                (c-or-c++-mode . c-or-c++-ts-mode)
;;                (bash-mode . bash-ts-mode)
;;                (css-mode . css-ts-mode)
;;                (json-mode . json-ts-mode)
;;                (js-json-mode . json-ts-mode)
;;                (sh-mode . bash-ts-mode)
;;                (sh-base-mode . bash-ts-mode)))
;;       (add-to-list 'major-mode-remap-alist mapping))
;;     :config
;;     (os/setup-install-grammars)
;;     ;; Do not forget to customize Combobulate to your liking:
;;     ;;
;;     ;;  M-x customize-group RET combobulate RET
;;     ;;
;;     (use-package! combobulate
;;       :preface
;;       ;; You can customize Combobulate's key prefix here.
;;       ;; Note that you may have to restart Emacs for this to take effect!
;;       (setq combobulate-key-prefix "C-c o")

;;       ;; Optional, but recommended.
;;       ;;
;;       ;; You can manually enable Combobulate with `M-x
;;       ;; combobulate-mode'.
;;       :hook
;;       ((python-ts-mode . combobulate-mode)
;;        (js-ts-mode . combobulate-mode)
;;        (go-mode . go-ts-mode)
;;        (html-ts-mode . combobulate-mode)
;;        (css-ts-mode . combobulate-mode)
;;        (yaml-ts-mode . combobulate-mode)
;;        (typescript-ts-mode . combobulate-mode)
;;        (json-ts-mode . combobulate-mode)
;;        (tsx-ts-mode . combobulate-mode))
;;       ;; Amend this to the directory where you keep Combobulate's source
;;       ;; code.
;;       :load-path ("~/workspace/combobulate")))
;;   )

;;   ;;;; Code Completion
;; (use-package! corfu
;;   :ensure t
;;   ;; Optional customizations
;;   :custom
;;   (corfu-cycle t)                 ; Allows cycling through candidates
;;   (corfu-auto t)                  ; Enable auto completion
;;   (corfu-auto-prefix 2)           ; Minimum length of prefix for completion
;;   (corfu-auto-delay 0)            ; No delay for completion
;;   (corfu-popupinfo-delay '(0.5 . 0.2))  ; Automatically update info popup after that numver of seconds
;;   (corfu-preview-current 'insert) ; insert previewed candidate
;;   (corfu-preselect 'prompt)
;;   (corfu-on-exact-match nil)      ; Don't auto expand tempel snippets
;;   ;; Optionally use TAB for cycling, default is `corfu-complete'.
;;   :bind (:map corfu-map
;;               ("M-SPC"      . corfu-insert-separator)
;;               ("TAB"        . corfu-next)
;;               ([tab]        . corfu-next)
;;               ("S-TAB"      . corfu-previous)
;;               ([backtab]    . corfu-previous)
;;               ("S-<return>" . corfu-insert)
;;               ("RET"        . corfu-insert))

;;   :init
;;   (global-corfu-mode)
;;   (corfu-history-mode)
;;   (corfu-popupinfo-mode) ; Popup completion info
;;   :config
;;   (add-hook 'eshell-mode-hook
;;             (lambda () (setq-local corfu-quit-at-boundary t
;;                                    corfu-quit-no-match t
;;                                    corfu-auto nil)
;;               (corfu-mode))
;;             nil
;;             t))

;; (after! lsp-mode
;;   (use-package! lsp-mode
;;     :diminish "LSP"
;;     :ensure t
;;     :hook ((lsp-mode . lsp-diagnostics-mode)
;;            (lsp-mode . lsp-enable-which-key-integration)
;;            ((tsx-ts-mode
;;              typescript-ts-mode
;;              js-ts-mode) . lsp-deferred))
;;     :custom
;;     (lsp-keymap-prefix "C-c l")           ; Prefix for LSP actions
;;     (lsp-completion-provider :none)       ; Using Corfu as the provider
;;     (lsp-diagnostics-provider :flycheck)
;;     (lsp-session-file (locate-user-emacs-file ".lsp-session"))
;;     (lsp-log-io nil)                      ; IMPORTANT! Use only for debugging! Drastically affects performance
;;     (lsp-keep-workspace-alive nil)        ; Close LSP server if all project buffers are closed
;;     (lsp-idle-delay 0.5)                  ; Debounce timer for `after-change-function'
;;     ;; core
;;     (lsp-enable-xref t)                   ; Use xref to find references
;;     (lsp-auto-configure t)                ; Used to decide between current active servers
;;     (lsp-eldoc-enable-hover t)            ; Display signature information in the echo area
;;     (lsp-enable-dap-auto-configure t)     ; Debug support
;;     (lsp-enable-file-watchers nil)
;;     (lsp-enable-folding nil)              ; I disable folding since I use origami
;;     (lsp-enable-imenu t)
;;     (lsp-enable-indentation nil)          ; I use prettier
;;     (lsp-enable-links nil)                ; No need since we have `browse-url'
;;     (lsp-enable-on-type-formatting nil)   ; Prettier handles this
;;     (lsp-enable-suggest-server-download t) ; Useful prompt to download LSP providers
;;     (lsp-enable-symbol-highlighting t)     ; Shows usages of symbol at point in the current buffer
;;     (lsp-enable-text-document-color nil)   ; This is Treesitter's job

;;     (lsp-ui-sideline-show-hover nil)      ; Sideline used only for diagnostics
;;     (lsp-ui-sideline-diagnostic-max-lines 20) ; 20 lines since typescript errors can be quite big
;;     ;; completion
;;     (lsp-completion-enable t)
;;     (lsp-completion-enable-additional-text-edit t) ; Ex: auto-insert an import for a completion candidate
;;     (lsp-enable-snippet t)                         ; Important to provide full JSX completion
;;     (lsp-completion-show-kind t)                   ; Optional
;;     ;; headerline
;;     (lsp-headerline-breadcrumb-enable t)  ; Optional, I like the breadcrumbs
;;     (lsp-headerline-breadcrumb-enable-diagnostics nil) ; Don't make them red, too noisy
;;     (lsp-headerline-breadcrumb-enable-symbol-numbers nil)
;;     (lsp-headerline-breadcrumb-icons-enable nil)
;;     ;; modeline
;;     (lsp-modeline-code-actions-enable nil) ; Modeline should be relatively clean
;;     (lsp-modeline-diagnostics-enable nil)  ; Already supported through `flycheck'
;;     (lsp-modeline-workspace-status-enable nil) ; Modeline displays "LSP" when lsp-mode is enabled
;;     (lsp-signature-doc-lines 1)                ; Don't raise the echo area. It's distracting
;;     (lsp-ui-doc-use-childframe t)              ; Show docs for symbol at point
;;     (lsp-eldoc-render-all nil)            ; This would be very useful if it would respect `lsp-signature-doc-lines', currently it's distracting
;;     ;; lens
;;     (lsp-lens-enable nil)                 ; Optional, I don't need it
;;     ;; semantic
;;     (lsp-semantic-tokens-enable nil)      ; Related to highlighting, and we defer to treesitter

;;     :init
;;     (setq lsp-use-plists t))

;;   :preface
;;   (defun lsp-booster--advice-json-parse (old-fn &rest args)
;;     "Try to parse bytecode instead of json."
;;     (or
;;      (when (equal (following-char) ?#)

;;        (let ((bytecode (read (current-buffer))))
;;          (when (byte-code-function-p bytecode)
;;            (funcall bytecode))))
;;      (apply old-fn args)))
;;   (defun lsp-booster--advice-final-command (old-fn cmd &optional test?)
;;     "Prepend emacs-lsp-booster command to lsp CMD."
;;     (let ((orig-result (funcall old-fn cmd test?)))
;;       (if (and (not test?)                             ;; for check lsp-server-present?
;;                (not (file-remote-p default-directory)) ;; see lsp-resolve-final-command, it would add extra shell wrapper
;;                lsp-use-plists
;;                (not (functionp 'json-rpc-connection))  ;; native json-rpc
;;                (executable-find "emacs-lsp-booster"))
;;           (progn
;;             (message "Using emacs-lsp-booster for %s!" orig-result)
;;             (cons "emacs-lsp-booster" orig-result))
;;         orig-result)))
;;   :init
;;   (setq lsp-use-plists t)
;;   ;; Initiate https://github.com/blahgeek/emacs-lsp-booster for performance
;;   (advice-add (if (progn (require 'json)
;;                          (fboundp 'json-parse-buffer))
;;                   'json-parse-buffer
;;                 'json-read)
;;               :around
;;               #'lsp-booster--advice-json-parse)
;;   (advice-add 'lsp-resolve-final-command :around #'lsp-booster--advice-final-command))


;; (after! lsp-completion
;;   (use-package! lsp-completion
;;     :no-require
;;     :hook ((lsp-mode . lsp-completion-mode)))
;;   )

;; (after! lsp-ui
;;   (use-package! lsp-ui
;;     :ensure t
;;     :commands
;;     (lsp-ui-doc-show
;;      lsp-ui-doc-glance)
;;     :bind (:map lsp-mode-map
;;                 ("C-c C-d" . 'lsp-ui-doc-glance))
;;     :after (lsp-mode evil)
;;     :config (setq lsp-ui-doc-enable t
;;                   evil-lookup-func #'lsp-ui-doc-glance ; Makes K in evil-mode toggle the doc for symbol at point
;;                   lsp-ui-doc-show-with-cursor nil      ; Don't show doc when cursor is over symbol - too distracting
;;                   lsp-ui-doc-include-signature t       ; Show signature
;;                   lsp-ui-doc-position 'at-point)))

;; (after! lsp-eslint
;;   (use-package! lsp-eslint
;;     :demand t
;;     :after lsp-mode))


;; (after! lsp-tailwindcss
;;   (use-package! lsp-tailwindcss
;;     :init (setq lsp-tailwindcss-add-on-mode t)
;;     :config
;;     (dolist (tw-major-mode
;;              '(css-mode
;;                css-ts-mode
;;                typescript-mode
;;                typescript-ts-mode
;;                tsx-ts-mode
;;                js2-mode
;;                js-ts-mode
;;                clojure-mode))
;;       (add-to-list 'lsp-tailwindcss-major-modes tw-major-mode))))
