;;; early-init.el --- Pre-package-initialization setup  -*- lexical-binding: t; -*-

;;; Commentary:

;; `package-enable-at-startup' is only consulted during early init.  Setting it
;; in ~/.emacs is too late: `package-activate-all' has already run and put every
;; directory in ~/.emacs.d/elpa on `load-path', shadowing the straight.el builds
;; of the same packages (vertico, consult, corfu, orderless, gruvbox-theme, ...).
;; That duplication is what caused intermittent completion breakage and the
;; "Unable to activate package 'eca'" / "Required package 's-1.12.0' is
;; unavailable" startup errors.  straight.el is the only package manager here.

;;; Code:

(setq package-enable-at-startup nil)

;;; early-init.el ends here
