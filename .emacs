;; .emacs --- Emacs Config.
;;; Commentary:
;;; Zach's Emacs configuration.
;;; Code:

;; Required for clipboard issues with Emacs >= 29 on macOS.  Emacs' own
;; clipboard integration is off; copy/paste go through `copy-selected-text' and
;; `paste-from-system-clipboard' (OSC 52 / pbcopy / xclip) further down.
;;
;; NOTE: `xterm-extra-capabilities' used to be set to nil here and then
;; overwritten unconditionally with a capability list in the XTERM-MOUSE
;; section below, so the nil never took effect.  The list is the behaviour this
;; config has actually been running with, so the dead nil is gone rather than
;; the list; if macOS clipboard trouble comes back, that list is the suspect.
(setq select-enable-clipboard nil)
(setq xterm-set-window-title nil)
(add-hook 'tty-setup-hook
          (lambda ()
            (setq interprogram-paste-function nil)
            (setq interprogram-cut-function nil)
            (when (fboundp 'xterm-osc-clipboard-mode)
	      (xterm-osc-clipboard-mode -1))))

;; package.el is disabled in early-init.el (it must be set before
;; `package-activate-all' runs).  straight.el manages everything here; never use
;; use-package's `:vc' keyword, which pulls package.el back in.

;; Add ~/.local/bin to exec-path for uv-installed tools (basedpyright, ruff, etc.)
(add-to-list 'exec-path (expand-file-name "~/.local/bin"))

;; utf-8
(set-language-environment "UTF-8")
(set-default-coding-systems 'utf-8)
;; no bell
(setq ring-bell-function 'ignore)
;; no-littering now lives after the straight.el bootstrap below, since
;; `use-package' cannot see the `:straight' keyword until then.
;; C-x U / L
(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)
;; line/col numbers
(setq display-line-numbers-type t)
(global-display-line-numbers-mode)
(column-number-mode)
;; Disable line numbers in terminal modes
(add-hook 'eat-mode-hook (lambda () (display-line-numbers-mode -1)))
(add-hook 'treemacs-mode-hook (lambda () (display-line-numbers-mode -1)))
(add-hook 'vterm-mode-hook (lambda () (display-line-numbers-mode -1)))
(add-hook 'term-mode-hook (lambda () (display-line-numbers-mode -1)))
(add-hook 'ghostel-mode-hook (lambda () (display-line-numbers-mode -1)))

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
       (expand-file-name
        "straight/repos/straight.el/bootstrap.el"
        (or (bound-and-true-p straight-base-dir)
            user-emacs-directory)))
      (bootstrap-version 7))
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

;; ---------- NO-LITTERING ----------
;; Organize auto-generated files.  Must come first among the use-package forms
;; so later packages' state files land under var/ and etc/ too.
(use-package no-littering
  :straight (no-littering :type git :host github
                          :repo "emacscollective/no-littering")
  :demand t
  :config
  ;; Store backup files in var/backup/
  (setq backup-by-copying t
        delete-old-versions t
        kept-new-versions 6
        kept-old-versions 2
        version-control t)
  ;; Configure auto-save and backup with no-littering paths
  (no-littering-theme-backups))

;; ---------- YAML-MODE ----------
(use-package yaml-mode
  :ensure t
  :mode ("\\.yml\\'" "\\.yaml\\'"))


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

;; ---------- TTY CHILD FRAMES ----------
;; https://lists.gnu.org/r/emacs-devel/2024-10/msg00491.html
;; Gerd Möllmann's tty-child-frames support lets posframe-based popups (corfu,
;; vertico-posframe, transient-posframe, which-key-posframe) work on ttys, not
;; just GUI frames.  As of Emacs 31 this is merged and `tty-child-frames' is
;; provided, and both posframe and corfu check for it themselves -- so the
;; `posframe-workable-p' / `corfu--popup-support-p' overrides this config used
;; to carry are gone.  They were byte-for-byte copies of upstream, and
;; redefining corfu's `cl-defgeneric' also discarded any `cl-defmethod' that
;; another package (corfu-terminal and friends) had registered on it.
;; Only the frame-parameter tuning below is still ours.
(when (>= emacs-major-version 31)
  (with-eval-after-load 'vertico-posframe
    (push '(tty-non-selected-cursor . t) vertico-posframe-parameters)
    (push '(undecorated . nil) vertico-posframe-parameters))

  (with-eval-after-load 'transient-posframe
    (push '(undecorated . nil) transient-posframe-parameters)))

;; ----------  MINIBUFFER COMPLETION (VERTICO/CONSULT vs IVY/COUNSEL) ----------
;; Emacs 31+ can render posframe child-frames on ttys (Gerd Möllmann's
;; tty-child-frames work, see the TTY CHILD FRAMES section above), so on
;; 31+ we switch to vertico/consult with vertico-posframe. Older Emacs
;; keeps the previous ivy/counsel/swiper setup.
(if (>= emacs-major-version 31)
    (progn
      (use-package posframe
	:ensure t)
      
      ;; consult commands routinely prompt from inside a minibuffer; without
      ;; this they fail with "Command attempted to use minibuffer while in
      ;; minibuffer".
      (setq enable-recursive-minibuffers t)
      (minibuffer-depth-indicate-mode 1)

      (use-package vertico
        :ensure t
        :init
        (vertico-mode)
        :config
        (setq vertico-count 14))

      (use-package vertico-posframe
        :ensure t
        :after vertico
        :config
        (vertico-posframe-mode 1))

      ;; Sorting and frecency only.  With filtering left on (the default),
      ;; `vertico-prescient-mode' advises `vertico--setup' to overwrite
      ;; `completion-styles' with `(prescient basic)' and to null out
      ;; `completion-category-defaults', which silently disables the orderless
      ;; config below and loses out-of-order word matching.
      (use-package vertico-prescient
        :ensure t
        :after vertico
        :init
        (setq vertico-prescient-enable-filtering nil)
        :config
        (vertico-prescient-mode t)
        (prescient-persist-mode t))

      (use-package orderless
        :ensure t
        :custom
        (completion-styles '(orderless basic))
        (completion-category-overrides '((file (styles basic partial-completion)))))

      (use-package marginalia
        :ensure t
        :init
        (marginalia-mode))

      (use-package consult
        :ensure t
        :bind (("C-s" . consult-line)
               ("C-r" . consult-line)
               ("M-y" . consult-yank-pop)
               ("C-x b" . consult-buffer)
               ("C-x 4 b" . consult-buffer-other-window)
               ("C-c C-r" . consult-history)))

      (use-package consult-projectile
        :ensure t
	:bind (("C-x B" . consult-projectile))
        :after (consult projectile)))

  (progn
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
      (setq ivy-use-selectable-prompt t)
      (setq ivy-count-format "(%d/%d) "))))

;; ---------- SHELL COMPLETION ----------
;; Bash completion support
(use-package bash-completion
  :ensure t
  :config
  (setq bash-completion-prog (executable-find "bash"))
  (bash-completion-setup))

;; Use bash-completion for all shells (works with bash, zsh via bash compatibility)
(defun my/shell-complete-command ()
  "Complete shell command at point using bash completion.
Returns completion data in the format expected by completion-at-point-functions."
  (when (and (boundp 'bash-completion-prog) bash-completion-prog)
    (let* ((start (save-excursion (beginning-of-line) (point)))
           (end (point))
           (completion-data (bash-completion-dynamic-complete-nocomint start end t)))
      ;; bash-completion-dynamic-complete-nocomint returns (start end collection)
      ;; completion-at-point-functions expects the same format
      completion-data)))

;; Setup completion-at-point in minibuffer for shell commands
(defun my/setup-shell-completion-minibuffer ()
  "Setup shell completion in minibuffer.
Installs a fresh keymap layered over the current one rather than calling
`local-set-key'.  In a minibuffer `current-local-map' is the *shared*
`minibuffer-local-map' (or `minibuffer-local-completion-map'), so
`local-set-key' mutates it in place and permanently rebinds TAB for every
later minibuffer in the session, breaking `vertico-insert' and filename
completion."
  (setq-local completion-at-point-functions
              (list #'my/shell-complete-command))
  (use-local-map
   (make-composed-keymap
    (let ((map (make-sparse-keymap)))
      (define-key map (kbd "TAB") #'completion-at-point)
      map)
    (current-local-map))))

;; Advice to add shell completion to commands
(defun my/shell-command-with-completion (orig-fun &rest args)
  "Advice to add shell completion to shell command prompts."
  (minibuffer-with-setup-hook
      #'my/setup-shell-completion-minibuffer
    (apply orig-fun args)))

;; Apply to standard shell commands
(advice-add 'shell-command :around #'my/shell-command-with-completion)
(advice-add 'async-shell-command :around #'my/shell-command-with-completion)

;; Apply to projectile shell commands
(with-eval-after-load 'projectile
  (advice-add 'projectile-run-command-in-root :around #'my/shell-command-with-completion)
  (advice-add 'projectile-run-shell-command-in-root :around #'my/shell-command-with-completion)
  (advice-add 'projectile-run-async-shell-command-in-root :around #'my/shell-command-with-completion))

;; ---------- GIT ----------
(use-package magit
  :ensure t
  :config
  (setq magit-module-section nil
	magit-section-initial-visibility-alist '((modules . show))))

(with-eval-after-load 'magit
  ;; Unbind M-1 through M-6 so winum keybindings work in magit buffers
  (define-key magit-mode-map (kbd "M-1") nil)
  (define-key magit-mode-map (kbd "M-2") nil)
  (define-key magit-mode-map (kbd "M-3") nil)
  (define-key magit-mode-map (kbd "M-4") nil)
  (define-key magit-mode-map (kbd "M-5") nil)
  (define-key magit-mode-map (kbd "M-6") nil))
(use-package el-mock
  :ensure t)
(use-package difftastic
  :straight (difftastic :type git :host github :repo "pkryger/difftastic.el")
  :config (difftastic-bindings-mode))

;; ---------- PROJECTILE ----------
(use-package projectile
  :ensure t
  :init
  (projectile-mode +1)
  :bind (:map projectile-mode-map
	      ("C-c p" . projectile-dispatch))
  :config
  (setq  projectile-enable-cmake-presets t
	 projectile-per-project-compilation-buffer t)

  (defun projectile-dispatch-find-file-fd ()
    "Find file in project using fd, ignoring .gitignore."
    (interactive)
    (let ((default-directory (projectile-acquire-root)))
      (find-file
       (completing-read
	"Find file (fd): "
	(split-string
	 (shell-command-to-string
	  (concat "fd --type f --no-require-git --no-ignore-vcs --hidden "
		  "-E '.git' -E '.venv' -E '.env'"))
	 "\n" t)))))

  (defun projectile-dispatch-magit ()
    "Run magit-status in the project root."
    (interactive)
    (let ((default-directory (projectile-acquire-root)))
      (magit-status)))

  (projectile--dispatch-define)
  (transient-replace-suffix 'projectile-dispatch "F"
    '("F" "file (fd, no ignore)" projectile-dispatch-find-file-fd))
  (transient-append-suffix 'projectile-dispatch "v"
    '("m" "magit" projectile-dispatch-magit))
  (transient-append-suffix 'projectile-dispatch "m"
    '("M" "magit dispatch" magit-dispatch))

  (defun projectile-dispatch-claude-code-ide ()
    "Run `claude-code-ide' in the project root."
    (interactive)
    (let ((default-directory (projectile-acquire-root)))
      (claude-code-ide)))

  (transient-append-suffix 'projectile-dispatch "cX"
    '("C" "claude-code-ide" projectile-dispatch-claude-code-ide))

  (defun projectile-dispatch-consult-ripgrep ()
    "Run `consult-ripgrep' in the project root."
    (interactive)
    (let ((default-directory (projectile-acquire-root)))
      (consult-ripgrep)))

  (transient-replace-suffix 'projectile-dispatch "s r"
    '("s r" "ripgrep" projectile-dispatch-consult-ripgrep))
  )


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
  ;; Was "C-c c", which collided with `copy-selected-text' further down; which
  ;; one won depended on whether cmake-integration had been loaded yet.
  (global-set-key (kbd "C-c k") 'cmake-integration-transient)
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
  (global-set-key (kbd "M-H") 'iflipb-previous-buffer)
  (global-set-key (kbd "C-M-h") 'ff-find-other-file))

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
  (treemacs-git-commit-diff-mode t)
  (treemacs-fringe-indicator-mode 'always)
  (require 'treemacs-project-follow-mode)
  (treemacs-project-follow-mode t)
  (setq treemacs-file-event-delay 1000
	treemacs-is-never-other-window t
	treemacs-silent-refresh t)
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
(use-package treemacs-nerd-icons
  :config
  (treemacs-nerd-icons-config))
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

;; ---------- DIRED ----------
(use-package dired-ranger
  :ensure t
  :straight (dired-hacks :type git :host github :repo "Fuco1/dired-hacks"))
(use-package dired-subtree
  :ensure t
  :straight (dired-hacks :type git :host github :repo "Fuco1/dired-hacks")
  ;; These were "C-S i" / "C-S r" / "C-S t".  There is no "C-S" modifier
  ;; prefix: (kbd "C-S i") evaluates to plain "i", so these bound i, r and t in
  ;; dired-mode-map, shadowing dired-maybe-insert-subdir, dired-do-rename and
  ;; dired-toggle-marks.
  :bind (:map dired-mode-map
	 ("C-c s i" . dired-subtree-insert)
	 ("C-c s r" . dired-subtree-remove)
	 ("C-c s t" . dired-subtree-toggle))
  )
(use-package dired-rainbow
  :ensure t
  :straight (dired-hacks :type git :host github :repo "Fuco1/dired-hacks")
  :config
  (progn
    (dired-rainbow-define directory "#6cb2eb" "d.*")))

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
    ;; `json-mode-beautify' takes (BEGIN END).  This used to pass 0 as BEGIN,
    ;; which is not a valid buffer position -- points are 1-based -- so every
    ;; C-x C-s in a JSON buffer signalled args-out-of-range.
    (json-mode-beautify (point-min) (point-max))
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
(use-package python-mode
  :mode "\\.py\\'")
;; ---------- MARKDOWN ----------
(use-package markdown-mode
  :ensure t
  :mode ("README\\.md\\'" . gfm-mode)
  :init (setq markdown-command "multimarkdown")
  :bind (:map markdown-mode-map
         ("C-c C-e" . markdown-do)))

;; ---------- ELIXIR ----------
(use-package elixir-mode
  :ensure t)
(use-package mix
  :config
  (add-hook 'elixir-mode-hook 'mix-minor-mode))
(straight-use-package
 '(exunit :type git :host github :repo "ananthakumaran/exunit.el"))
(use-package exunit
  :ensure t
  :config
  (add-hook 'elixir-mode-hook 'exunit-mode))

;; ---------- TREESITTER ----------
(use-package treesit-auto
  :config
  (global-treesit-auto-mode))

;; ----------- COMPANY / LSP MODE / FLYCHECK ---------------
(use-package flycheck
  :ensure t
  :init (global-flycheck-mode)
  :config
  ;; Disable flycheck-ruff for Python - ruff-lsp provides diagnostics via LSP
  (add-hook 'python-mode-hook
            (lambda ()
              (setq-local flycheck-disabled-checkers '(python-ruff)))))

;; in-buffer completion: corfu (Emacs 31+, posframe/tty-child-frames capable)
;; vs. company (older Emacs)
(if (>= emacs-major-version 31)
    (progn
      (use-package corfu
        :ensure t
        :init
        (global-corfu-mode)
        :custom
        (corfu-cycle t)
        (corfu-auto t)
        (corfu-auto-delay 0)
        (corfu-auto-prefix 2)
        (corfu-quit-no-match 'separator))

      (use-package corfu-prescient
        :ensure t
        :after corfu
        :config
        (corfu-prescient-mode t)
        (prescient-persist-mode t)))

  (progn
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
      (prescient-persist-mode t))))

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
	 (c++-ts-mode . lsp)
	 (c-mode . lsp)
	 (c-ts-mode . lsp)
	 (elixir-mode . lsp)
	 (lsp-mode . lsp-enable-which-key-integration))
  :config
  (define-key lsp-mode-map (kbd "C-c l") lsp-command-map)

  (require 'lsp-clients)
  (setq
   lsp-log-io nil
   lsp-idle-delay 0.500
   ;; performance stuff
   lsp-prefer-flymake nil
   lsp-enable-snippet t)
  ;; `lsp-treemacs-sync-mode' is a minor mode, not a variable: setq'ing it to 1
  ;; flipped the flag without ever running the body that installs the hooks.
  (with-eval-after-load 'lsp-treemacs
    (lsp-treemacs-sync-mode 1))
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
;; Bind into `lsp-command-map', not `lsp-mode-map'.  "C-c l" is already bound
;; to the whole lsp-command-map above, so defining "C-c l g s" in the same
;; keymap tries to use a non-prefix key as a prefix.
(if (>= emacs-major-version 31)
    (use-package consult-lsp
      :ensure t
      :after (consult lsp-mode)
      :bind (:map lsp-command-map
                  ("g s" . consult-lsp-file-symbols)
                  ("g S" . consult-lsp-symbols)
                  ("g d" . consult-lsp-diagnostics)))
  (use-package lsp-ivy
    :commands lsp-ivy-workspace-symbol))

;; ---------- DEBUGGER ----------
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

;; ---------- C/C++ MODE FORMATTING ----------
(defun my/c-mode-common-hook ()
  "Use clang-format for C/C++ formatting."
  ;; Basic display settings
  (setq c-basic-offset 3)
  (setq indent-tabs-mode nil)
  (setq tab-width 3)
  (setq-local lsp-clients-clangd-executable "clangd")
  (setq-local clang-format-executable "/usr/bin/clang-format-20")
  (clang-format-on-save-mode)
  (local-set-key (kbd "C-M-h") 'ff-find-other-file)
  (require 'bazel)
  (when-let* ((result (locate-dominating-file buffer-file-name
                                              #'bazel--workspace-root-p)))
    ;; Configure LSP to use bazel-clangd-wrapper for this bazel workspace
    (setq-local lsp-clients-clangd-executable "bazel-clangd-wrapper")
    (setq-local lsp-clients-clangd-args '("--clangd-path=clangd-20"))
    )
  ;; Format buffer with clang-format before saving
  ;; (add-hook 'before-save-hook 'clang-format-buffer nil t)
  )

(add-hook 'c-mode-hook 'my/c-mode-common-hook)
(add-hook 'c++-mode-hook 'my/c-mode-common-hook)
(add-hook 'c-ts-mode-hook 'my/c-mode-common-hook)
(add-hook 'c++-ts-mode-hook 'my/c-mode-common-hook)

(setq lsp-cmake-server-command (expand-file-name "~/.local/bin/cmake-language-server"))

;; ---------- RUST ----------
(use-package rustic
  :ensure t)

;; ---------- DOCKERFILE ----------
(use-package dockerfile-mode
  :ensure t)

;; -------- WHICH KEY MODE ---------
(use-package which-key
    :config
    (which-key-mode)
    (setq which-key-idle-delay 0.25))

(when (>= emacs-major-version 31)
  (use-package which-key-posframe
    :ensure t
    :after which-key
    :config
    (which-key-posframe-mode 1)))

(use-package transient
  :ensure t
  :config
  (setq transient-show-menu 0.01))

;; -------- TRANSIENT POSFRAME ---------
(when (>= emacs-major-version 31)
  (use-package transient-posframe
    :ensure t
    :after transient
    :config
    (transient-posframe-mode 1)))

;; -------- ORG MODE ----------------
;; org mode
(use-package org
  :mode (("\\.org$" . org-mode))
  :ensure org-plus-contrib
  :config
  (setq org-todo-keywords
	'((sequence "TODO" "IN-PROGRESS" "VERIFY" "|" "DONE" "DELEGATED" "CANCELLED"))
	org-directory (expand-file-name "~/org/"))
  (load-library "find-lisp")
  (setq org-agenda-files
	(find-lisp-find-files org-directory "\.org$")
	org-babel-python-command "python3")
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((emacs-lisp . t)
     (shell . t)
     (python . t))))

;; Set the browser in emacs
(if (getenv "BROWSER")
    (setq browse-url-generic-program
	  (executable-find (getenv "BROWSER"))
	  browse-url-browser-function 'browse-url-generic))

;; `update-mobile-files' below uses both f- and s- functions; only f was
;; required, which is where the "Required package 's-1.12.0' is unavailable"
;; startup error came from.
(use-package f :ensure t :demand t)
(use-package s :ensure t :demand t)

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
  (global-tree-sitter-mode -1))

;; ---------- HURL ------------
(straight-use-package
 '(hurl-mode :type git :host github :repo "jaszhe/hurl-mode"))
(use-package hurl-mode
  :mode "\\.hurl\\'")

;; ---------- BAZEL ----------
(straight-use-package
 '(bazel :type git :host github :repo "zacharyasmith/emacs-bazel-mode"))
(use-package bazel
  :ensure t)


;; ---------- TRAMP ----------
(setq tramp-default-method "ssh")

;; counsel-tramp does `(require 'counsel)', which drags the whole ivy stack in
;; alongside vertico.  It also had no deferring keyword, so use-package loaded
;; it at startup.  On Emacs 31 use consult-tramp instead; keep counsel-tramp
;; (deferred) on older builds where ivy/counsel is the completion UI anyway.
(if (>= emacs-major-version 31)
    (use-package consult-tramp
      :straight (consult-tramp :type git :host github :repo "Ladicle/consult-tramp")
      :bind ("C-c C-s" . consult-tramp))
  (use-package counsel-tramp
    :ensure t
    :bind ("C-c C-s" . counsel-tramp)))

;; --------- SERVER ----------
;; Needed so emacsclient (and `with-editor' below) can hand files to *this*
;; Emacs instead of starting a new one.
(require 'server)
(unless (server-running-p)
  (server-start))

;; --------- WITH-EDITOR ----------
;; Keep $EDITOR from opening a *nested* terminal Emacs inside a ghostel buffer.
;; A plain "emacsclient -nw" makes a new tty frame on the terminal it was
;; invoked from -- which, inside ghostel, is the ghostel pty.  The result is an
;; Emacs painted inside an Emacs buffer, with the outer Emacs swallowing every
;; keystroke.  `with-editor-export-editor' instead exports an $EDITOR that hands
;; the file to this Emacs via the server and blocks the child until C-x #.
;; So e.g. claude-code's prompt editor opens a normal window here.
(use-package with-editor
  :ensure t
  :hook ((ghostel-mode . with-editor-export-editor)
         (eat-mode . with-editor-export-editor)
         (vterm-mode . with-editor-export-editor)
         (term-exec . with-editor-export-editor)
         (eshell-mode . with-editor-export-editor)))

;; --------- CLAUDE ----------
(use-package ghostel
  :ensure t
  :bind (("C-x m" . ghostel)
         :map ghostel-semi-char-mode-map
         ("C-s"  . consult-line)
         ("C-k"  . my/ghostel-send-C-k-and-kill)
         ;; I'm used to go up/down the shell history with M-n/p from eshell
         ;; Simulate this behavior in ghostel by sending C-p and C-n
         ("M-p" . (lambda () (interactive) (ghostel-send-key "p" "ctrl")))
         ("M-n" . (lambda () (interactive) (ghostel-send-key "n" "ctrl")))
         :map project-prefix-map
         ("m" . ghostel-project)
         ("M" . ghostel-project-list-buffers)))

(setq
   ghostel-term "xterm-direct")
(defun my/ghostel-send-C-k-and-kill ()
    "Send `C-k' to ghostel.
Like normal Emacs `C-k'.  Kill to end of line and put content in kill-ring."
    (interactive)
    (kill-ring-save (point) (line-end-position))
    (ghostel-send-key "k" "ctrl"))

(add-to-list 'project-switch-commands '(ghostel-project "Ghostel") t)
(add-to-list 'project-switch-commands '(ghostel-project-list-buffers "Ghostel buffers") t)
;; (add-to-list 'ghostel-eval-cmds '("magit-status-setup-buffer" magit-status-setup-buffer))
(ghostel-compile-global-mode)
(ghostel-eshell-visual-command-mode)
(ghostel-comint-global-mode)
(use-package claude-code-ide
  :straight (:type git :host github :repo "manzaltu/claude-code-ide.el")
  :bind (("C-c '" . claude-code-ide-menu)
         ("C-c C-i" . claude-code-ide-implement-todo))
  :ensure t
  :config
  (setq claude-code-ide-terminal-backend 'ghostel
	claude-code-ide-enable-mcp-server t
	claude-code-ide-use-side-window t
	claude-code-ide-focus-on-open t
	claude-code-ide-show-claude-window-in-ediff nil
	claude-code-ide-window-width 100
	claude-code-ide-prevent-reflow-glitch t)

  (defun my-project-grep (pattern)
    "Search for PATTERN in the current session's project."
    (claude-code-ide-mcp-server-with-session-context nil
      ;; This executes with the session's project directory as default-directory
      (let* ((project-dir default-directory)
             (results (shell-command-to-string
                       (format "rg -n '%s' %s" pattern project-dir))))
	results)))

  ;; Define and register the tool (automatically added to claude-code-ide-mcp-server-tools)
  (claude-code-ide-make-tool
   :function #'my-project-grep
   :name "my_project_grep"
   :description "Search for pattern in project files"
   :args '((:name "pattern"
		  :type string
		  :description "Pattern to search for")))

  (claude-code-ide-emacs-tools-setup)

  (defun claude-code-ide-implement-todo ()
    "Send the current TODO line to Claude with context about the current buffer.
This function:
1. Captures the current line (assumed to contain a TODO)
2. Switches to the claude-code-ide buffer
3. Sends a prompt asking Claude to check the Emacs MCP for the current buffer
4. Asks Claude to implement the TODO
5. Automatically submits the prompt"
    (interactive)
    (save-buffer)
    (let* ((current-buffer-name (buffer-name))
           (current-file-name (buffer-file-name))
           (current-line (string-trim (thing-at-point 'line t)))
           (line-number (line-number-at-pos)))
      (let* ((prompt (format "Look at the Emacs MCP to see which file is currently open. Then implement this TODO:\n\n%s\n\n(from %s:%d)"
			     current-line
			     (or current-file-name current-buffer-name)
			     line-number)))
        ;; Switch to the Claude buffer first
        (claude-code-ide-switch-to-buffer)
        ;; Send the prompt (this will automatically submit it)
        (claude-code-ide-send-prompt prompt))))

  (defconst claude-code-ide--small-frame-width-threshold 170
    "Full-screen terminal width, in columns, on the 13\" laptop.
Frames no wider than this are treated as a single small screen,
where a side window would crowd out the rest of the editor.")

  (define-advice claude-code-ide--display-buffer-in-side-window
      (:around (orig-fn buffer) no-side-window-on-small-frame)
    "Open in a full-frame buffer instead of a side window on small frames."
    (if (or (not claude-code-ide-use-side-window)
            (> (frame-width) claude-code-ide--small-frame-width-threshold))
        (funcall orig-fn buffer)
      (let* ((claude-code-ide-use-side-window nil)
             (display-buffer-alist
              (cons `(,(regexp-quote (buffer-name buffer))
                       (display-buffer-full-frame))
                    display-buffer-alist)))
        (funcall orig-fn buffer)))))

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

(use-package eat
  :ensure t
  :config
  ;; Add M-1 through M-6 to non-bound keys so winum bindings work in semi-char
  ;; mode.  The variable is `eat-semi-char-non-bound-keys' (plural); this used
  ;; to assign to `...-non-bound-key', which nothing reads.
  (setq eat-semi-char-non-bound-keys
        (append eat-semi-char-non-bound-keys
                '([M-1] [M-2] [M-3] [M-4] [M-5] [M-6])))
  (eat-update-semi-char-mode-map))

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

(defun paste-from-system-clipboard ()
  (interactive)
  (let ((text
         (cond
          ((eq system-type 'darwin)
           (shell-command-to-string "pbpaste"))
          ((and (eq system-type 'gnu/linux) (not (display-graphic-p)))
           (user-error "In a terminal, paste with Cmd+V (terminal paste)"))
          ((eq system-type 'gnu/linux)
           (shell-command-to-string "xclip -selection clipboard -o"))
          ((or (eq system-type 'windows-nt)
               (string-match-p "WSL" (or (getenv "WSL_DISTRO_NAME") "")))
           (shell-command-to-string "powershell.exe -command Get-Clipboard")))))
    (when text
      (insert (string-trim-right text)))))

(defun osc52-copy (text)
  "Copy TEXT to the outer terminal's clipboard via OSC 52.
Works over ssh, mosh, and tmux (with `set-clipboard on') without X."
  (send-string-to-terminal
   (concat "\e]52;c;"
           (base64-encode-string (encode-coding-string text 'utf-8) t)
           "\a")))

(defun copy-selected-text (start end)
  (interactive "r")
  (when (use-region-p)
    (let ((text (buffer-substring-no-properties start end)))
      (cond
       ((eq system-type 'darwin)
        (with-temp-buffer
          (insert text)
          (call-process-region (point-min) (point-max) "pbcopy")))
       ((and (eq system-type 'gnu/linux) (not (display-graphic-p)))
        (osc52-copy text))
       ((eq system-type 'gnu/linux)
        (with-temp-buffer
          (insert text)
          (call-process-region (point-min) (point-max) "xclip" nil nil nil "-selection" "clipboard")))
       ((or (eq system-type 'windows-nt)
            (string-match-p "WSL" (or (getenv "WSL_DISTRO_NAME") "")))
        (with-temp-buffer
          (insert text)
          (call-process-region (point-min) (point-max) "clip.exe")))))))

(global-set-key (kbd "C-c c") 'copy-selected-text)
(global-set-key (kbd "C-c v") 'paste-from-system-clipboard)

;; ---------- RAINBOW ----------
(use-package csv-mode
  :defer t)
(straight-use-package
 '(rainbow-csv-mode :type git :host github :repo "emacs-vs/rainbow-csv"))
(use-package rainbow-csv-mode
  :defer t
  :mode ("\\.csv\\'" . rainbow-csv-mode))

;; ---------- HEXL ----------
(use-package nhexl-mode
  :ensure t)

;; Rainbow colorization for hexl-mode
(use-package rainbow-hexl-mode
  :straight nil  ; Local package, not from a repository
  :load-path "~/dotfiles"
  :commands (rainbow-hexl-mode rainbow-hexl-refontify)
  :after hexl
  :hook (hexl-mode . rainbow-hexl-mode)
  :custom
  (rainbow-hexl-saturation 0.8)
  (rainbow-hexl-min-lightness 0.5)
  (rainbow-hexl-max-lightness 1.0))

;; ---------- TTY IMAGES ----------
;; Show images in terminal Emacs (locally or over ssh) on terminals
;; with the kitty graphics protocol, e.g. Ghostty.  GUI frames still
;; use the normal image-mode.
(use-package tty-image
  :straight nil  ; Local package, not from a repository
  :load-path "~/dotfiles"
  :demand t
  :commands (tty-image-insert-file)
  :config
  (tty-image-mode 1))

;; ---------- MULTI-CURSORS ----------
(use-package iy-go-to-char
  :ensure t
  :bind (("C-c f" . 'iy-go-to-char)
	 ("C-c F" . 'iy-go-to-char-backward)
	 ("C-c ;" . 'iy-go-to-or-up-to-continue)
	 ("C-c ," . 'iy-go-to-or-up-to-continue-backward)))
(use-package multiple-cursors
  :ensure t
  :bind (("C-c m" . 'mc/edit-lines)
	 ("C-." . 'mc/mark-next-like-this)
	 ("C-," . 'mc/mark-previous-like-this)
	 ("C-M-." . 'mc/mark-next-like-this-word)
	 ("C-M-," . 'mc/mark-previous-like-this-word)
	 ("C-c C-," . 'mc/mark-all-like-this))
  :config
  (add-to-list 'mc/cursor-specific-vars 'iy-go-to-char-start-pos))


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

;; ---------- VTERM ----------
(use-package vterm
    :ensure t)

(defun my/vterm-with-completion (command)
  "Run COMMAND with rlwrap in a new vterm buffer.
Uses bash completion for command input."
  (interactive
   (list
    (minibuffer-with-setup-hook
        #'my/setup-shell-completion-minibuffer
      (read-string "Command: "))))
  (let ((vterm-shell (format "%s" command))
        (buffer-name (format "*vterm-%s*" command)))
    (vterm buffer-name)))

;; ---------- EDIT-INDIRECT ----------
(use-package edit-indirect
  :straight (:type git :host github :repo "Fanael/edit-indirect"))


;; ---------- HELP PAGE ----------

(defvar help-page--exec-cache nil
  "Cached list of executables found in `exec-path'.")

(defvar help-page--exec-cache-time nil
  "Time at which `help-page--exec-cache' was last populated.")

(defvar help-page-cache-ttl 300
  "Seconds before the executable cache is refreshed.")

(defun help-page--executables ()
  "Return a sorted, deduplicated list of executables on `exec-path'.
Results are cached for `help-page-cache-ttl' seconds."
  (if (and help-page--exec-cache
           help-page--exec-cache-time
           (< (float-time (time-subtract nil help-page--exec-cache-time))
              help-page-cache-ttl))
      help-page--exec-cache
    (setq help-page--exec-cache-time (current-time)
          help-page--exec-cache
          (cl-remove-duplicates
           (sort
            (cl-loop for dir in exec-path
                     when (and dir (file-directory-p dir))
                     nconc (cl-loop for f in (directory-files dir nil nil t)
                                    when (and (not (member f '("." "..")))
                                              (file-executable-p (expand-file-name f dir))
                                              (not (file-directory-p (expand-file-name f dir))))
                                    collect f))
            #'string<)
           :test #'string=))))

(defun help-page-invalidate-cache ()
  "Force the executable cache to be refreshed on next `help-page' call."
  (interactive)
  (setq help-page--exec-cache nil
        help-page--exec-cache-time nil)
  (message "help-page executable cache cleared."))

(defun help-page ()
  "Display --help output for an executable, formatted with bat.
Executables are discovered from `exec-path' and presented via
`completing-read'.  Output is piped through bat for syntax
highlighting and displayed in a read-only buffer (special-mode)."
  (interactive)
  (let* ((cmd (completing-read "Help for: " (help-page--executables) nil t))
         (buf-name (format "*help: %s*" cmd))
         (output (shell-command-to-string
                  (format "%s --help 2>&1 | bat --color always --pager never -l cmd-help --style=plain"
                          (shell-quote-argument cmd)))))
    (with-current-buffer (get-buffer-create buf-name)
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert output)
        (ansi-color-apply-on-region (point-min) (point-max))
        (goto-char (point-min)))
      (special-mode))
    (pop-to-buffer buf-name)))
(global-set-key (kbd "C-h c") #'help-page)
(global-set-key (kbd "C-h M") #'man)


;; STARTUP!
(add-hook 'after-init-hook (lambda ()
  (org-agenda-list)
  (delete-other-windows)))

(use-package breadcrumb
  :ensure t
  :config (breadcrumb-mode t))

;; ---------- MACHINE-LOCAL CONFIG ----------
;; Load ~/.emacs.d/custom/init.el if present (work-specific packages/settings)
(let ((custom-init (expand-file-name "custom/init.el" user-emacs-directory)))
  (when (file-exists-p custom-init)
    (load custom-init)))

;; custom variables
(put 'projectile-project-package-cmd 'safe-local-variable #'stringp)
(put 'projectile-project-compilation-cmd 'safe-local-variable #'stringp)
(put 'projectile-project-run-cmd 'safe-local-variable #'stringp)
(put 'projectile-project-configure-cmd 'safe-local-variable #'stringp)
(put 'projectile-project-test-cmd 'safe-local-variable #'stringp)
(put 'dockerfile-image-name 'safe-local-variable #'stringp)
(put 'projectile-project-root 'safe-local-variable #'stringp)

;; ---------- CUSTOM FILE ----------
;; Keep Custom's writes out of this file: it is tracked in dotfiles, and
;; machine-specific values (e.g. `safe-local-variable-directories' pointing at
;; work checkouts) do not belong in version control.
(setq custom-file (expand-file-name "custom-vars.el" user-emacs-directory))
(when (file-exists-p custom-file)
  (load custom-file))

(provide '.emacs)
;;; .emacs ends here
