;;; .emacs --- Emacs Config.
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

;; hookup with straight use-package
(straight-use-package 'use-package)
(setq straight-use-package-by-default t)
;; https://github.com/jwiegley/use-package#getting-started
(use-package use-package-ensure-system-package
  :ensure t)

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

;; ---------- SMART SHIFT ----------
(straight-use-package
 '(smart-shift :type git :host github :repo "hbin/smart-shift"))
(use-package smart-shift)
(key-chord-define-global "<<" 'smart-shift-left)
(key-chord-define-global ">>" 'smart-shift-right)

;; ---------- THEME ----------
;; zenburn https://github.com/bbatsov/zenburn-emacs
(use-package zenburn-theme
  :ensure t
  :load-path "themes"
  :config
  (load-theme 'zenburn t)
  ;; use variable-pitch fonts for some headings and titles
  (setq zenburn-use-variable-pitch t)
  ;; scale headings in org-mode
  (setq zenburn-scale-org-headlines t)
  ;; scale headings in outline-mode
  (setq zenburn-scale-outline-headlines t))

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
  :ensure t)

;; ---------- PROJECTILE ----------
(use-package projectile
  :ensure t
  :config
  (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)
  (setq projectile-completion-system 'ivy)
  (projectile-mode +1))

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

;; ---------- PROJECTILE MODE ----------
(use-package projectile
  :ensure t
  :init
  (projectile-mode +1)
  :bind (:map projectile-mode-map
              ("s-p" . projectile-command-map)
              ("C-c p" . projectile-command-map)))

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
(use-package python-pytest)
(defun python-save-buffer ()
    "Format before save."
    (interactive)
    (lsp-format-buffer)
    (save-buffer))
(add-hook 'python-mode-hook
	  (lambda ()
	    (local-set-key (kbd "C-c C-t") 'python-pytest-dispatch)
	    (local-set-key (kbd "C-x C-s") 'save-buffer)))

;; ----------- COMPANY / LSP MODE / FLYCHECK ---------------
(use-package flycheck
  :ensure t
  :init (global-flycheck-mode))

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
(use-package lsp-mode
  :init
  ;; set prefix for lsp-command-keymap
  (setq lsp-keymap-prefix "C-c l")
  :hook (;; replace xxx-mode with concrete major-mode(e. g. python-mode)
         (python-mode . lsp)
	 (json-mode . lsp)
	 (rust-mode . lsp)
	 (lsp-mode . lsp-enable-which-key-integration))
  :config
  (setq
   lsp-log-io nil
   lsp-pylsp-plugins-flake8-enabled nil
   lsp-pylsp-plugins-pydocstyle-enabled nil
   lsp-pylsp-plugins-mccabe-enabled nil
   lsp-pylsp-plugins-rope-completion-enabled t
   lsp-pylsp-plugins-yapf-enabled t
   lsp-idle-delay 0.500
   lsp-pylsp-rename-backend 'rope
   ;; performance stuff
   lsp-prefer-flymake nil
   lsp-enable-snippet t)
  :commands lsp
  :ensure-system-package
  (pylsp . "python -m pip install python-lsp-server[yapf,flake8,rope,pydocstyle]"))
(remove-hook 'python-mode-hook 'lsp)
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
;; debugger
(use-package dap-mode
  :after lsp-mode
  :commands dap-debug
  :hook ((python-mode . dap-ui-mode)
	 (python-mode . dap-mode))
  :config
  (eval-when-compile
    (require 'cl))
  (require 'dap-python)
  (require 'dap-lldb)
  )
;; language server plugin
(use-package python-mode
  :mode "\\.py\\'")

;; ---------- RUST ----------
(use-package rustic
  :ensure t)

;; ---------- PHP ----------
(use-package php-mode
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

;; ---------- WS BUTLER ----------
(use-package ws-butler
  :ensure t
  :config
  (ws-butler-global-mode 1))

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


;; ---------- TRAMP ----------
(use-package counsel-tramp
  :ensure t
  :config
  (setq tramp-default-method "ssh")
  (define-key global-map (kbd "C-c C-s") 'counsel-tramp))
(use-package docker-tramp
  :ensure t)

;; ---------- VTERM ----------
(use-package vterm
  :ensure t
  :ensure-system-package
  (cmake . "brew install cmake"))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:inherit nil :stipple nil :background "#3F3F3F" :foreground "#DCDCCC" :inverse-video nil :box nil :strike-through nil :extend nil :overline nil :underline nil :slant normal :weight normal :height 130 :width normal :foundry "nil" :family "FiraCode Nerd Font")))))
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
 '(python-pytest-executable "poetry run pytest --capture=tee-sys"))

(provide '.emacs)
;;; .emacs ends here
