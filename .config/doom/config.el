;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Reconfigure packages with `after!' block wrap, otherwise Doom's defaults may override your settings. E.g.
;;   (after! PACKAGE
;;     (setq x y))
;;
;; Exceptions to this:
;;   - Setting file/directory variables (like `org-directory')
;;   - Setting variables which explicitly tell you to set them before their
;;     package is loaded (see 'C-h v VARIABLE' to look up their documentation).
;;   - Setting doom variables (which start with 'doom-' or '+').
;;
;; Additional Doom configuration functions/macros:
;; - `load!' for loading external *.el files relative to this one
;; - `use-package!' for configuring packages
;; - `after!' for running code after a package has loaded
;; - `add-load-path!rectories to the `load-path', relative to
;;   this file. Emacs searches the `load-path' when you load packages with
;;   `require' or `use-package'.
;; - `map!' for binding new keys
;;
;; See documentation about these functions/macros, by pressing
;; 'K' over the highlighted symbol ('C-c c k' for non-evil users).
;; open documentation for it, including demos of how they are used.
;; Alternatively, use `C-h o' to look up symbols (functions, variables, faces,
;; etc).
;;
;; See implementations with 'gd' over symbol (or 'C-c c d').

;; https://tecosaur.github.io/emacs-config


(after! dired
  (setq dirvish-peek-mode t)
  (setq dirvish-side-auto-close t))

(setq display-line-numbers-type 'relative)

(use-package! gptel ;; LLM Integration
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

(setq browse-url-browser-function 'browse-url-default-browser) ;; Set default browser

(setq engine/search-engine 'google) ;; Set default search engine to Google

(setq projectile-project-search-path '("~/workspace/" "~/Documents/")) ;; Projectile search directories
(setq dirvish-side-auto-close t)  ;; Auto-close when focus is lost

;;; :editor evil
(setq evil-split-window-below t ;; Focus new window after splitting
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

  ;; Custom TODO colors
  (setq org-todo-keyword-faces '(("TODO" :foreground "#7c7c75" :weight normal :underline t)
                                 ("INPROGRESS" :foreground "#0098dd" :weight normal :underline t)
                                 ("DONE" :foreground "#50a14f" :weight normal :underline t)
                                 ("CANCELLED" :foreground "#ff6480" :weight normal :underline t)
                                 ("BLOCKED" :foreground "#ff9800" :weight normal :underline t)))

  ;; Custom priority colors
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

(after! magit
  (setq magit-diff-refine-hunk 'all)

  (use-package! magit-todos
    :config (magit-todos-mode 1))

  (use-package! magit-file-icons
    :config (magit-file-icons-mode 1)
    )
  (setq magit-log-margin-show-committer-date t)
  ;; (setq magit-log-arguments '("--graph" "--color" "--decorate" "-n256"))
  (setq magit-section-visibility-indicator '(" " . " "))

  (setq magit-revision-insert-related-refs t)
  (add-hook 'magit-mode-hook 'hl-line-mode)
  (add-hook 'magit-mode-hook 'display-line-numbers-mode)
  )

(custom-set-faces
 '(magit-diff-added ((t (:foreground "#00ff00" :background "#002200"))))
 '(magit-diff-removed ((t (:foreground "#ff0000" :background "#220000"))))
 '(magit-section-heading ((t (:foreground "#ffff00" :weight bold))))
 '(magit-diff-context ((t (:foreground "#b0b0b0"))))
 '(magit-diff-hunk-heading ((t (:background "#3a3f5a"))))
 '(magit-diff-hunk-heading-highlight ((t (:background "#51576d" :foreground "#ffffff")))))

;; TODO: Read Better alternatives for M-x, from 'Effective Emacs' by Steve Yegge: https://sites.google.com/site/steveyegge2/effective-emacs

;; When viewing a pdf, view it in dark mode instead of the default light mode
(add-hook
 'pdf-view-mode-hook
 'pdf-view-midnight-minor-mode)

(after! pdf-tools
  (setq pdf-view-continuous t))

(use-package! org-pandoc-import :after org)

(defun my/switch-to-last-buffer-in-split ()
  "Switch to the lats buffer and split screen."
  (interactive)
  (let ((current-buffer (current-buffer)))
    (if (one-window-p)
        (progn
          (split-window-right)
          (other-window 1)
          (switch-to-buffer current-buffer)
          (other-window -1)
          (evil-switch-to-windows-last-buffer)
          ))
    )
  )

(map! :leader ;; Remap switching to last buffer from 'SPC+`' to 'SPC+e'
      :desc "Switch to last buffer" "e"
      ;; #'evil-switch-to-windows-last-buffer)
      #'my/switch-to-last-buffer-in-split)

(map! :leader
      ;; ";" #'vterm
      "d" #'copilot-chat-custom-prompt-selection)

(minimap-mode)

(after! vterm
  (set-popup-rule! "*doom:vterm-popup:*" :size 0.5 :vslot -4 :select t :quit nil :ttl 0 :side 'right)
  (add-hook 'vterm-mode-hook #'evil-normal-state) ;; tart vterm in normal mode
  )

;; Display relative line numbers
(menu-bar--display-line-numbers-mode-relative)
(setq display-line-numbers-type t)

(nyan-mode)

;; (after! modeline
;; :config
;; (setq doom-modeline-height 25)
(setq doom-modeline-hud t)
(setq doom-modeline-persp-name t)
;; )

;; (map! :leader "g g" nil) ;; Unbind default Magit
(map! :leader
      "g l" (lambda ()
              (interactive)
              (let ((window-config (current-window-configuration))) ;; Save current layout
                (delete-other-windows)  ;; Maximize window for vterm
                (vterm "*lazygit*")
                (vterm-send-string "lazygit; exit") ;; Exit vterm when lazygit exits
                (vterm-send-return)

                ;; Restore layout when vterm process exits
                (set-process-sentinel
                 (get-buffer-process "*lazygit*")
                 (lambda (_process _event)
                   (when (buffer-live-p (get-buffer "*lazygit*"))
                     (kill-buffer "*lazygit*")) ;; Kill vterm buffer
                   (set-window-configuration window-config)))))) ;; Restore layout

;; (setq org-gcal-client-id "some-client-id.apps.googleusercontent.com"
;;       org-gcal-client-secret "some-client-secret"
;;       org-gcal-fetch-file-alist '(("someemail@gmail.com" .  "~/Documents/schedule.org")
;;                                   ))

(setq plstore-cache-passphrase-for-symmetric-encryption t)

;; :which-key
(setq which-key-idle-delay 0.25) ;; Make popup faster
(setq which-key-allow-multiple-replacements t) ;; Remove 'evil-' in too many popups
(after! which-key
  (pushnew!
   which-key-replacement-alist
   '(("" . "\\`+?evil[-:]?\\(?:a-\\)?\\(.*\\)") . (nil . "◂\\1"))
   '(("\\`g s" . "\\`evilem--?motion-\\(.*\\)") . (nil . "◃\\1"))
   ))

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!

(setq fancy-splash-image "~/dotfiles/.config/doom/doom-emacs-color2.svg") ;; Custom banner
(after! doom-dashboard ;; Custom text
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
(setq doom-theme
      'doom-lantern
      ;; 'doom-gruvbox
      ;; 'doom-gruvbox-light
      ;; 'doom-solarized-light
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

(setq user-full-name "Mumtahin Farabi"
      user-mail-address "mfarabi619@gmail.com")

;; https://www.ovistoica.com/blog/2024-7-05-modern-emacs-typescript-web-tsx-config

;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
(setq org-directory "~/Documents/")
