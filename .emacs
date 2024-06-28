(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(company-show-numbers (quote (quote t)))
 '(custom-enabled-themes (quote (deeper-blue)))
 '(package-selected-packages
   (quote
    (exec-path-from-shell projectile csv-mode less-css-mode cmake-mode company-irony irony))))

(add-to-list 'load-path "~/.emacs.d/lisp")

;; replace on yank
(delete-selection-mode 1)

;; auto-complete
;(ac-config-default)

;; turn off dings!!!
(setq visible-bell 1)
(global-display-line-numbers-mode)
(column-number-mode)

()

;; packages
(defun z-package-refresh nil
  "Refresh package contents from MELPA"
  (interactive)
  (require 'package)
  (package-initialize)
  (add-to-list 'package-archives
	       '("melpa-stable" . "https://stable.melpa.org/packages/") t)
  (package-refresh-contents))
(z-package-refresh)

;; import shell variables from own setup
(exec-path-from-shell-initialize)

(add-to-list 'load-path "~/.emacs.d/beancount-mode/")
(require 'beancount)
(add-to-list 'auto-mode-alist '("\\.beancount\\'" . beancount-mode))

;; projectile
(require 'projectile)
(projectile-mode +1)
(define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)

;; create a new frame
(global-set-key (kbd "M-n M-f") 'make-frame)
(global-set-key (kbd "<f8>") 'other-frame)
(global-set-key (kbd "C-i") 'company-complete)
(global-set-key (kbd "<C-tab>") 'indent-for-tab-command)

;; irony mode
(when (boundp 'w32-pipe-buffer-size)
  (setq irony-server-w32-pipe-buffer-size (* 64 1024)))
(add-hook 'c++-mode-hook 'irony-mode)
(add-hook 'c-mode-hook 'irony-mode)
(add-hook 'irony-mode-hook 'irony-cdb-autosetup-compile-options)

;; C++
(require 'custom-c-mode)
(defun z-c-set-style ()
  ""
  (interactive)
  (c-set-style "z"))
(add-hook 'c++-mode-hook 'z-c-set-style)

;; cmake
(require 'cmake-project)
(setq cmake-project-default-build-dir-name "build/")
(defun maybe-cmake-project-mode ()
  (if (or (file-exists-p "CMakeLists.txt")
	  (file-exists-p (expand-file-name "CMakeLists.txt" (car (project-roots (project-current))))))
      (cmake-project-mode)))
;; cmake project
(add-hook 'c-mode-hook 'maybe-cmake-project-mode)
(add-hook 'c++-mode-hook 'maybe-cmake-project-mode)
(add-hook 'cmake-project-mode-hook
	  (lambda () (local-set-key (kbd "C-c C-c") 'compile)))

;; python
(setq python-shell-interpreter "/usr/local/bin/python3")

;; company
(add-hook 'after-init-hook 'global-company-mode)
(eval-after-load 'company
  '(add-to-list 'company-backends 'company-irony))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

(server-start)
(put 'downcase-region 'disabled nil)
(put 'upcase-region 'disabled nil)
