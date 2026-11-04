# .emacs review remediation

## Stage 1: Terminal color depth
**Goal**: gruvbox renders in 24-bit instead of collapsing to 8 ANSI colors
**Success Criteria**: inside tmux, `(display-color-cells)` reports 16777216
**Tests**: `M-x list-colors-display`; faded/neutral/bright gruvbox reds differ
**Status**: Complete

- `.tmux.conf` tracked in dotfiles with `default-terminal "tmux-256color"` + `terminal-features ...:RGB` (done by user)
- `~/.tmux.conf` symlink was self-referential; repointed at absolute path

## Stage 2: Stop package.el shadowing straight
**Goal**: one copy of each package on `load-path`
**Success Criteria**: no `Unable to activate package 'eca'` / `s-1.12.0 is unavailable` at startup
**Tests**: `--debug-init` startup log clean; `(locate-library "vertico")` points into straight/build
**Status**: Not Started

- add `early-init.el` with `package-enable-at-startup nil`
- `:vc` -> `:straight` for no-littering and difftastic
- drop `package-vc-selected-packages` from `custom-set-variables`
- move `~/.emacs.d/elpa` to `~/.emacs.d/elpa.bak`

## Stage 3: Minibuffer completion correctness
**Goal**: vertico behaves identically every invocation
**Success Criteria**: TAB in minibuffer still completes after running `M-!`; out-of-order word matching works
**Tests**: `M-!` then `C-x C-f` and press TAB; `M-x` with "fi fi" matching find-file
**Status**: Not Started

- stop `my/setup-shell-completion-minibuffer` mutating shared `minibuffer-local-map`
- resolve prescient vs orderless (`vertico-prescient-enable-filtering nil`)
- defer `counsel-tramp` so ivy/counsel do not load on Emacs 31
- drop obsolete tty-child-frames shims

## Stage 4: Editor handoff out of ghostel
**Goal**: `$EDITOR` from a ghostel buffer opens in the parent Emacs, not nested inside the buffer
**Success Criteria**: claude-code prompt edit opens a window in the outer Emacs and blocks until `C-x #`
**Tests**: in a ghostel buffer run `$EDITOR /tmp/x`; confirm no nested tty Emacs
**Status**: Not Started

- `server-start` in `.emacs`
- `with-editor-export-editor` on `ghostel-mode-hook`
- `EDITOR`/`VISUAL` without `-nw` in the `INSIDE_EMACS=ghostel` branch of `.bashrc`

## Stage 5: Outright bugs
**Goal**: remove bindings and setqs that silently do nothing or break a mode
**Success Criteria**: byte-compiles without warnings; dired `i`/`r`/`t` work again
**Tests**: open dired, press `t`; `C-x C-s` in a .json buffer; `M-1` in an eat buffer
**Status**: Not Started

- dired `C-S i` -> valid chord (currently binds plain `i`/`r`/`t`)
- `eat-semi-char-non-bound-key` -> `...-keys`
- `treemacs-mode` -> `treemacs-mode-hook`
- `when-let` -> `when-let*`
- `json-mode-beautify 0` -> `(point-min) (point-max)`
- `setq lsp-treemacs-sync-mode` -> call the minor mode
- consult-lsp keys into `lsp-command-map`
- `C-c c` double binding
- missing `(require 's)`
- `xterm-extra-capabilities` contradiction

## Stage 6: Cleanup
**Goal**: quieter, faster startup
**Success Criteria**: no tsc-dyn-get / grammar-bundle / treemacs-resize lines in startup log
**Tests**: `emacs --debug-init -nw` log
**Status**: Not Started

- drop legacy `tree-sitter` + `tree-sitter-langs`
- drop external `python-mode` package
- `warning-minimum-level :error`
- `custom-file` out of `.emacs`
- guard `treemacs-resize-icons`
- dead bindings: `company-yasnippet`, `python-pytest-dispatch`, `python-save-buffer`
- `org-plus-contrib`, `load-library` -> `require`
- `nov` duplicate `:config`
