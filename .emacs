;; .emacs --- Emacs Config.
;;; Commentary:
;;; Zach's Emacs configuration.
;;; Code:

(setq package-enable-at-startup nil)

;; utf-8
(set-language-environment "UTF-8")
(set-default-coding-systems 'utf-8)
;; no bell
(setq ring-bell-function 'ignore)
;; C-x U / L
(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)
;; line/col numbers
(global-display-line-numbers-mode)
(column-number-mode)
;; delete selection with paste
(delete-selection-mode 1)
;; (when (daemonp)
;;   (exec-path-from-shell-initialize))
;; new frames
(global-set-key (kbd "M-n M-f") 'make-frame)
(global-set-key (kbd "<f8>") 'other-frame)
;; highlight current line
(global-hl-line-mode t)
;; We don't want to type yes and no all the time so, do y and n
(defalias 'yes-or-no-p 'y-or-n-p)
;; Disable the menu bar since we don't use it, especially not in the
;; terminal
(when (and (not (eq system-type 'darwin)) (fboundp 'menu-bar-mode))
  (menu-bar-mode -1))
;; Non-nil means draw block cursor as wide as the glyph under it.
;; For example, if a block cursor is over a tab, it will be drawn as
;; wide as that tab on the display.
(setq x-stretch-cursor t)
;; Dont ask to follow symlink in git
(setq vc-follow-symlinks t)
(unless (display-graphic-p)
  (xterm-mouse-mode 1))
(setq xterm-extra-capabilities '(getSelection setSelection modifyOtherKeys))
;; Check (on save) whether the file edited contains a shebang, if yes,
;; make it executable from
;; http://mbork.pl/2015-01-10_A_few_random_Emacs_tips
(add-hook 'after-save-hook #'executable-make-buffer-file-executable-if-script-p)
;; Highlight some keywords in prog-mode
(add-hook 'prog-mode-hook
          (lambda ()
            ;; Highlighting in cmake-mode this way interferes with
            ;; cmake-font-lock, which is something I don't yet understand.
            (when (not (derived-mode-p 'cmake-mode))
              (font-lock-add-keywords
               nil
               '(("\\<\\(FIXME\\|TODO\\|BUG\\|DONE\\)"
                  1 font-lock-warning-face t))))))

;; install straight
(setq warning-minimum-level :emergency)
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 6))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

(straight-use-package 'org)
(savehist-mode)

;; hookup with straight use-package
(straight-use-package 'use-package)
(setq straight-use-package-by-default t)

;; ---------- YAML-MODE ----------
(use-package yaml-mode
  :ensure t
  :mode (("\\.ya?ml\\'" . yaml-mode)))


;; -------- CADDYFILE -----------
(use-package caddyfile-mode
  :ensure t
  :mode (("Caddyfile\\'" . caddyfile-mode)
         ("caddy\\.conf\\'" . caddyfile-mode)))

;; ---------- HIGHLIGHT-INDENTATION ----------
(use-package highlight-indentation
  :mode (("\\.ya?ml\\'" . highlight-indentation-mode))
  :mode (("\\.py\\'" . highlight-indentation-mode))
  :config
    (set-face-background 'highlight-indentation-face "#808075")
    (set-face-background 'highlight-indentation-current-column-face "#BFBFB0")
    )

;; ---------- KEY-CHORD ----------
(straight-use-package 'key-chord)
(key-chord-mode 1)

;; ---------- THEME ----------
;; zenburn https://github.com/bbatsov/zenburn-emacs
 (use-package gruvbox-theme
   :ensure t
   :config
 (load-theme 'gruvbox-dark-hard t))


;; Hide the scroll bar
(if (fboundp 'scroll-bar-mode)
    (scroll-bar-mode -1))

;; mood-line
(straight-use-package
 '(mood-line :type git :host github :repo "jessiehildebrandt/mood-line"))
(use-package mood-line
  :ensure t
  :config
  (mood-line-mode)
  (setq mood-line-show-encoding-information t))

;; ----------  IVY/COUNSEL ----------
(use-package counsel
  :ensure t
  :bind (("C-s" . swiper-isearch)
	 ("C-r" . swiper-isearch-backward)
	 ("M-x" . counsel-M-x)
	 ("C-x C-f" . counsel-find-file)
	 ("M-y" . counsel-yank-pop)
	 ("C-h f" . counsel-describe-function)
	 ("C-h v" . counsel-describe-variable)
	 ("<f1> l" . counsel-find-library)
	 ("<f2> i" . counsel-info-lookup-symbol)
	 ("<f2> u" . counsel-unicode-char)
	 ("<f2> j" . counsel-set-variable)
	 ("C-x b" . ivy-switch-buffer)
	 ("C-c v" . ivy-push-view)
	 ("C-c V" . ivy-pop-view)
	 ("C-c C-r" . ivy-resume)
	 ("C-x 4 b" . ivy-switch-buffer-other-window))
  :config
  (ivy-mode 1)
  (use-package ivy-prescient
    :ensure t
    :after (counsel)
    :config
    (ivy-prescient-mode t)
    (prescient-persist-mode t)
    )
  (use-package counsel-projectile
    :ensure t
    :after (:all counsel projectile)
    :bind (("C-x M-f" . counsel-projectile-find-file-dwim))
    :init
    (eval-when-compile
      ;; Silence missing function warnings
      (declare-function counsel-projectile-mode "counsel-projectile.el"))
    :config
    (counsel-projectile-mode))
  (setq ivy-use-virtual-buffers t)
  (setq ivy-count-format "(%d/%d) "))

;; ---------- GIT ----------
(use-package magit
  :ensure t
  :config
  (setq magit-module-section nil
	magit-section-initial-visibility-alist '((modules . show))))
(use-package difftastic
  :ensure t
  :vc (:url "https://github.com/pkryger/difftastic.el.git"
	    :rev :newest)
  :config (difftastic-bindings-mode))

;; ---------- RIPGREP ----------
(use-package ripgrep
  :ensure t)

;; ---------- PROJECTILE ----------
(use-package projectile
  :ensure t
  :init
  (projectile-mode +1)
  :bind (:map projectile-mode-map
	      ("C-c p" . projectile-command-map))
  :config
  (setq projectile-completion-system 'ivy
	projectile-enable-cmake-presets t
	projectile-per-project-compilation-buffer t))

(use-package projectile-ripgrep
  :ensure t)

;; ---------- CMAKE/CONAN ----------
(straight-use-package
 '(cmake-integration :type git :host github :repo "darcamo/cmake-integration"))
(use-package cmake-integration
  :ensure t
  :commands (cmake-integration-conan-manage-remotes
             cmake-integration-conan-list-packages-in-local-cache
             cmake-integration-search-in-conan-center
             cmake-integration-transient)
  :config
  ;; (cmake-integration-generator "Gnu")
  (setq cmake-integration-use-separated-compilation-buffer-for-each-target t)
  (global-set-key (kbd "C-c c") 'cmake-integration-transient)
  ;; :bind (:map c++-mode-map
  ;;             ([f5] . cmake-integration-transient) ;; Open main transient menu
  ;;             ([M-f9] . cmake-integration-select-current-target) ;; Ask for target
  ;;             ([f9] . cmake-integration-save-and-compile-last-target) ;; Recompile last target
  ;;             ([C-f9] . cmake-integration-run-ctest) ;; Run CTest
  ;;             ([f10] . cmake-integration-run-last-target) ;; Run last target (with saved args)
  ;;             ([S-f10] . kill-compilation) ;; Stop compilation
  ;;             ([C-f10] . cmake-integration-debug-last-target) ;; Debug last target
  ;;             ([M-f10] . cmake-integration-run-last-target-with-arguments) ;; Run last target with custom args
  ;;             ([M-f8] . cmake-integration-select-configure-preset) ;; Select and configure preset
  ;;             ([f8] . cmake-integration-cmake-reconfigure) ;; Reconfigure with last preset
  ;;             )
  )

;; ---------- WINUM ----------
(use-package winum
  :ensure t
  :config
  (winum-mode)
  (global-set-key (kbd "M-1") 'winum-select-window-1)
  (global-set-key (kbd "M-2") 'winum-select-window-2)
  (global-set-key (kbd "M-3") 'winum-select-window-3)
  (global-set-key (kbd "M-4") 'winum-select-window-4)
  (global-set-key (kbd "M-5") 'winum-select-window-5)
  (global-set-key (kbd "M-6") 'winum-select-window-6)
  )
(use-package iflipb
  :ensure t
  :config
  (global-set-key (kbd "M-h") 'iflipb-next-buffer)
  (global-set-key (kbd "M-H") 'iflipb-previous-buffer))

;; --------- TREEMACS ---------
(use-package treemacs
  :ensure t
  :defer t
  :init
  (with-eval-after-load 'winum
    (define-key winum-keymap (kbd "M-0") #'treemacs-select-window))
  :config
  (treemacs-resize-icons 18)
  (treemacs-follow-mode t)
  (treemacs-filewatch-mode t)
  (treemacs-fringe-indicator-mode 'always)
  (when treemacs-python-executable
    (treemacs-git-commit-diff-mode t))
  (pcase (cons (not (null (executable-find "git")))
               (not (null treemacs-python-executable)))
    (`(t . t)
     (treemacs-git-mode 'deferred))
    (`(t . _)
     (treemacs-git-mode 'simple)))
  (treemacs-hide-gitignored-files-mode nil)
  :bind
  (:map global-map
        ("M-0"       . treemacs-select-window)
        ("C-x t 1"   . treemacs-delete-other-windows)
        ("C-x t t"   . treemacs)
        ("C-x t d"   . treemacs-select-directory)
        ("C-x t B"   . treemacs-bookmark)
        ("C-x t C-t" . treemacs-find-file)
        ("C-x t M-t" . treemacs-find-tag))
  )

(use-package treemacs-projectile
  :after (treemacs projectile)
  :ensure t
  :bind
  (:map treemacs-project-map
	("C-c C-p a" . treemacs-add-project-to-workspace)
	("C-c C-p p" . treemacs-projectile)
	("C-c C-p d" . treemacs-remove-project-from-workspace)
	("C-c C-p c o" . treemacs-collapse-all-projects)))

(use-package treemacs-icons-dired
  :hook (dired-mode . treemacs-icons-dired-enable-once)
  :ensure t)

(use-package treemacs-magit
  :after (treemacs magit)
  :ensure t)


;; ---------- XTERM ------------------
(use-package eterm-256color
  :hook (term-mode . eterm-256color-mode))
(use-package xterm-color
  :config
  (setq compilation-environment '("TERM=xterm-256color"))
  (defun my/advice-compilation-filter (f proc string)
    (funcall f proc (xterm-color-filter string)))

  (advice-add 'compilation-filter :around #'my/advice-compilation-filter))

;; ----------- JSON MODE ---------------
(use-package json-mode
  :ensure t
  :mode (("\\.json$" . json-mode)))
(use-package json-reformat
  :ensure t)
(straight-use-package
 '(json-snatcher :type git :host github :repo "Sterlingg/json-snatcher"))
(use-package json-snatcher
  :ensure t)
(defun json-save-buffer ()
    "Format before save."
    (interactive)
    (json-mode-beautify 0 (buffer-end 1))
    (save-buffer))
(add-hook 'json-mode-hook
	  (lambda ()
	    (local-set-key (kbd "C-x C-s") 'json-save-buffer)))

;; ----------- PYTHON MODE ---------------
;; (use-package python-pytest)
(defun python-save-buffer ()
    "Format before save."
    (interactive)
    (lsp-format-buffer)
    (save-buffer))
(add-hook 'python-mode-hook
	  (lambda ()
	    (local-set-key (kbd "C-c C-t") 'python-pytest-dispatch)
	    (local-set-key (kbd "C-x C-s") 'save-buffer)))
;; language server plugin
(require 'project)
(use-package pyvenv
  :ensure t
  :config
  ;; Function to find and activate uv virtual environment
  (defun activate-uv-venv ()
    "Activate uv virtual environment for current project."
    (interactive)
    (let* ((project-root (or (locate-dominating-file default-directory ".venv")
			     (locate-dominating-file default-directory "pyproject.toml")
			     ))
           (venv-path (when project-root
			(expand-file-name ".venv" project-root))))
      (when (and venv-path (file-exists-p venv-path))
	(pyvenv-activate venv-path)
	(message "Activated uv virtual environment: %s" venv-path))))
  )
(use-package lsp-pyright
  :ensure t
  :hook (python-mode
	 .(lambda ()
	    (activate-uv-venv)
            (setq lsp-ruff-server-command "NAN"
		  lsp-pyright-langserver-command "basedpyright"
		  lsp-pyright-use-library-code-for-types t
		  lsp-pyright-disable-language-service nil
		  lsp-pyright-disable-organize-imports nil)
            (lsp))))
(use-package python-mode
  :mode "\\.py\\'")
;; ---------- MARKDOWN ----------
(use-package markdown-mode
  :ensure t
  :mode ("README\\.md\\'" . gfm-mode)
  :init (setq markdown-command "multimarkdown")
  :bind (:map markdown-mode-map
         ("C-c C-e" . markdown-do)))

;; ----------- COMPANY / LSP MODE / FLYCHECK ---------------
(use-package flycheck
  :ensure t
  :init (global-flycheck-mode)
  :config
  (setq
   flycheck-python-ruff-executable "ruff"))

;; company mode
(use-package company
  :ensure t
  :config
  (global-company-mode 1)
  (setq company-idle-delay 0)
  :init
  (setq
   company-minimum-prefix-length 2
   company-tooltip-limit 14
   company-tooltip-align-annotations t
   company-require-match 'never

   ;; These auto-complete the current selection when
   ;; `company-auto-complete-chars' is typed. This is too magical. We
   ;; already have the much more explicit RET and TAB.
   company-auto-complete nil
   company-auto-complete-chars nil

   ;; Only search the current buffer for `company-dabbrev' (a backend that
   ;; suggests text your open buffers). This prevents Company from causing
   ;; lag once you have a lot of buffers open.
   company-dabbrev-other-buffers nil

   ;; Make `company-dabbrev' fully case-sensitive, to improve UX with
   ;; domain-specific words with particular casing.
   company-dabbrev-ignore-case nil
   company-dabbrev-downcase nil
   ))

(use-package company-prescient
  :ensure t
  :after company
  :config
  (company-prescient-mode t)
  (prescient-persist-mode t))

;; language server protocol
;; (use-package lsp-bridge
;;   :straight '(lsp-bridge :type git :host github :repo "manateelazycat/lsp-bridge"
;;             :files (:defaults "*.el" "*.py" "acm" "core" "langserver" "multiserver" "resources")
;;             :build (:not compile))
;;   :init
;;   (global-lsp-bridge-mode))

(use-package lsp-mode
  :init
  ;; set prefix for lsp-command-keymap
  (setq lsp-keymap-prefix "C-c l")
  :hook (;; replace xxx-mode with concrete major-mode(e. g. python-mode)
	 (json-mode . lsp)
	 (rust-mode . lsp)
	 (c++-mode . lsp)
	 (lsp-mode . lsp-enable-which-key-integration))
  :config
  (define-key lsp-mode-map (kbd "C-c l") lsp-command-map)
  (require 'lsp-clients)
  (setq 
   lsp-log-io nil
   lsp-idle-delay 0.500
   lsp-treemacs-sync-mode 1
   ;; performance stuff
   lsp-prefer-flymake nil
   lsp-enable-snippet t)
  :commands lsp
  )

(use-package lsp-ui
  :config (setq lsp-ui-sideline-show-hover nil
		lsp-ui-sideline-show-symbol t
                lsp-ui-sideline-delay 0.25
                lsp-ui-doc-delay 0.5
                lsp-ui-doc-position 'bottom
                lsp-ui-doc-alignment 'frame
                lsp-ui-doc-header nil
                lsp-ui-doc-include-signature t
                lsp-ui-doc-use-childframe t)
  :commands lsp-ui-mode)
(use-package lsp-ivy
  :commands lsp-ivy-workspace-symbol)
debugger
(use-package dap-mode
  :after lsp-mode
  :commands dap-debug
  :hook ((python-mode . dap-ui-mode)
	 (python-mode . dap-mode))
  :config
  (require 'dap-python)
  (require 'dap-lldb)
  (require 'dap-cpptools)
  (setq gdb-many-windows t
	gdb-show-main t
        gdb-debug-log-max 1024)
  )
;; C++
(use-package clang-format
  :ensure t
  :config
  (setq clang-format-fallback-style "llvm"))
(setq lsp-clangd-binary-path "/usr/bin/clangd-20"
      lsp-clangd-version "20.1.8"
      lsp-cmake-server-command "/home/zach/.venv/cmake-language-server/bin/cmake-language-server")

;; ---------- RUST ----------
(use-package rustic
  :ensure t)

;; ---------- DOCKERFILE ----------
(use-package dockerfile-mode
  :ensure t)

;; ---------- COMPANY ----------
;; company mode
(use-package company
  :ensure t
  :config
  (global-company-mode 1)
  (setq company-idle-delay 0)
  :init
  (setq
   company-minimum-prefix-length 2
   company-tooltip-limit 14
   company-tooltip-align-annotations t
   company-require-match 'never

   ;; These auto-complete the current selection when
   ;; `company-auto-complete-chars' is typed. This is too magical. We
   ;; already have the much more explicit RET and TAB.
   company-auto-complete nil
   company-auto-complete-chars nil

   ;; Only search the current buffer for `company-dabbrev' (a backend that
   ;; suggests text your open buffers). This prevents Company from causing
   ;; lag once you have a lot of buffers open.
   company-dabbrev-other-buffers nil

   ;; Make `company-dabbrev' fully case-sensitive, to improve UX with
   ;; domain-specific words with particular casing.
   company-dabbrev-ignore-case nil
   company-dabbrev-downcase nil
   ))
(use-package company-prescient
  :ensure t
  :after company
  :config
  (company-prescient-mode t)
  (prescient-persist-mode t)
  )


;; -------- WHICH KEY MODE ---------
(use-package which-key
    :config
    (which-key-mode)
    (setq which-key-idle-delay 0.25))

;; -------- ORG MODE ----------------
;; org mode
(use-package org
  :mode (("\\.org$" . org-mode))
  :ensure org-plus-contrib
  :config
  (setq org-todo-keywords
	'((sequence "TODO" "IN-PROGRESS" "VERIFY" "|" "DONE" "DELEGATED"))
	org-directory "~/tower/Zach/org/")
  (load-library "find-lisp")
  (setq org-agenda-files
	(find-lisp-find-files org-directory "\.org$")))

;; Set the browser in emacs
(if (getenv "BROWSER")
    (setq browse-url-generic-program
	  (executable-find (getenv "BROWSER"))
	  browse-url-browser-function 'browse-url-generic))

(require 'f)
(defun update-mobile-files nil
  "Do the remote update"
  (interactive)
  (setq org-mobile-files nil)
  (dolist (item (f-files org-directory (lambda (file) (and (not (s-matches? "flagged.org$" file)) (s-matches? ".org$" file))) t))
    (add-to-list 'org-mobile-files item)))
(advice-add 'org-mobile-push :before #'update-mobile-files)

(require 'ox-latex)
(unless (boundp 'org-latex-classes)
  (setq org-latex-classes nil))
(add-to-list 'org-latex-classes
             '("article"
               "\\documentclass{article}"
               ("\\section{%s}" . "\\section*{%s}")))

;; ---------- YASNIPPET ----------
(use-package yasnippet
  :ensure t
  :init
  (eval-when-compile
    ;; Silence missing function warnings
    (declare-function yas-global-mode "yasnippet.el"))
  :config
  (yas-reload-all)
  (add-hook 'prog-mode-hook #'yas-minor-mode)
  ;; Add snippet support to lsp mode
  (setq lsp-enable-snippet t)
  (define-key yas-minor-mode-map (kbd "<tab>") nil)
  (define-key yas-minor-mode-map (kbd "TAB") nil)
  :bind (("C-M-y" . company-yasnippet)
	 ("C-c y" . yas-expand))
  )
(use-package yasnippet-snippets
  :ensure t
  :after yasnippet
  :config
  (yas-reload-all))


;; ---------- SMART PARENS ----------
(use-package smartparens
  :ensure t
  :config
  (show-paren-mode 0)
  (smartparens-global-mode 1)
  (require 'smartparens-config)
  (define-key smartparens-mode-map (kbd "C-M-f") 'sp-forward-sexp)
  (define-key smartparens-mode-map (kbd "C-M-b") 'sp-backward-sexp)
  (define-key smartparens-mode-map (kbd "C-M-n") 'sp-down-sexp)
  (define-key smartparens-mode-map (kbd "C-M-p") 'sp-up-sexp))

;; ---------- DWARF MODE ----------
(let ((custom-file "~/dotfiles/dwarf-mode.el"))
  (when (file-exists-p custom-file)
    (load custom-file)))
(require 'dwarf-mode)

(defun my/inspect-shared-symbols (file)
  "Show global/dynamic symbols in the shared object FILE."
  (interactive "fShared object or static library: ")
  (let ((buf (get-buffer-create "*Symbols*")))
    (with-current-buffer buf
      (read-only-mode -1)
      (erase-buffer)
      (let ((exit-code
             (call-process "nm" nil buf nil "-gDC" file)))
        (if (not (zerop exit-code))
            (call-process "readelf" nil buf nil "-WsC" file)))
      (goto-char (point-min)))  ;; if you define or reuse a mode for nm-like listing
    (pop-to-buffer buf)))


;; ---------- RAINBOW DELIMITERS ----------
(use-package rainbow-delimiters
  :ensure t
  :hook
  ((emacs-lisp-mode . rainbow-delimiters-mode)
   (python-mode . rainbow-delimiters-mode)
   (php-mode . rainbow-delimiters-mode)
   (json-mode . rainbow-delimiters-mode)))

;; ---------- TREE SITTER ----------
(use-package tree-sitter
  :ensure t)
(use-package tree-sitter-langs
  :ensure t
  :config
  (global-tree-sitter-mode))

;; ---------- HURL ------------
(straight-use-package
 '(hurl-mode :type git :host github :repo "jaszhe/hurl-mode"))
(use-package hurl-mode
  :mode "\\.hurl\\'")

;; ---------- TRAMP ----------
(use-package counsel-tramp
  :ensure t
  :config
  (setq tramp-default-method "ssh")
  (define-key global-map (kbd "C-c C-s") 'counsel-tramp))

;; --------- CLAUDE ----------
(use-package claude-code-ide
  :straight (:type git :host github :repo "manzaltu/claude-code-ide.el")
  :bind ("C-c '" . claude-code-ide-menu) ; Set your favorite keybinding
  :config
  (claude-code-ide-emacs-tools-setup)
  (setq claude-code-ide-terminal-backend 'eat
	claude-code-ide-enable-mcp-server t)) ; Optionally enable Emacs MCP tools

;; ---------- EAT ----------
(straight-use-package
 '(eat :type git
       :host codeberg
       :repo "akib/emacs-eat"
       :files ("*.el" ("term" "term/*.el") "*.texi"
               "*.ti" ("terminfo/e" "terminfo/e/*")
               ("terminfo/65" "terminfo/65/*")
               ("integration" "integration/*")
               (:exclude ".dir-locals.el" "*-tests.el"))))

;; ---------- EPUB ----------
(use-package nov
  :demand t
  :config
  :mode "\\.epub\\'"
  :config
  (defun my-nov-font-setup ()
    (face-remap-add-relative 'variable-pitch :family "DejaVu Serif"
                             :height 1.0))
  (setq
   nov-text-width 80
   visual-fill-column-center-text t)
  (add-hook 'nov-mode-hook 'my-nov-font-setup)
  )

(defun copy-selected-text (start end)
  (interactive "r")
    (if (use-region-p)
        (let ((text (buffer-substring-no-properties start end)))
            (shell-command (concat "echo '" text "' | clip.exe")))))

;; ---------- RAINBOW ----------
(use-package csv-mode
  :ensure t)
(straight-use-package
 '(rainbow-csv-mode :type git :host github :repo "emacs-vs/rainbow-csv"))
(use-package rainbow-csv-mode
  :ensure t
  :mode "\\.csv\\'")

;; ---------- UUID ----------
(straight-use-package
 '(insert-uuid :type git :host github :repo "theesfeld/insert-uuid"))
(use-package insert-uuid
  :ensure t
  :bind (("C-c u" . insert-uuid)
         ("C-c U" . insert-uuid-random))
  :custom
  (insert-uuid-default-version 4)
  (insert-uuid-uppercase nil))

;; custom variables
(put 'projectile-project-package-cmd 'safe-local-variable #'stringp)
(put 'projectile-project-compilation-cmd 'safe-local-variable #'stringp)
(put 'projectile-project-run-cmd 'safe-local-variable #'stringp)
(put 'projectile-project-configure-cmd 'safe-local-variable #'stringp)
(put 'dockerfile-image-name 'safe-local-variable #'stringp)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(column-number-mode t)
 '(global-display-line-numbers-mode t)
 '(mood-line-show-encoding-information t)
 '(notmuch-saved-searches
   '((:name "inbox" :query "tag:inbox" :key "i")
     (:name "unread" :query "tag:unread" :key "u")
     (:name "flagged" :query "tag:flagged" :key "f")
     (:name "sent" :query "tag:sent" :key "t")
     (:name "drafts" :query "tag:draft" :key "d")
     (:name "all mail" :query "*" :key "a")
     (:name "omscs-orientation" :query "tag:omscs and tag:orientation")))
 '(package-vc-selected-packages
   '((difftastic :url "https://github.com/pkryger/difftastic.el.git")))
 '(python-pytest-executable "poetry run pytest --capture=tee-sys")
 '(safe-local-variable-values
   '((cmake-integration-current-target . "coverage")
     (projectile-project-package-cmd function stringp)
     (projectile-project-compilation-cmd function stringp)
     (projectile-project-run-cmd function stringp)
     (projectile-project-configure-cmd function stringp)
     (dockerfile-image-name function stringp))))

(provide '.emacs)
;;; .emacs ends here
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
