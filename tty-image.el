;;; tty-image.el --- Show images in terminal Emacs via the kitty graphics protocol -*- lexical-binding: t -*-

;; Author: Claude & User
;; Version: 1.0
;; Keywords: multimedia, terminals

;;; Commentary:

;; Terminal Emacs can't draw images, but terminals that speak the
;; kitty graphics protocol (Ghostty, kitty, WezTerm) can.  This
;; package transmits a PNG to the terminal with escape codes, then
;; inserts Unicode placeholder characters (U+10EEEE) whose foreground
;; color encodes the image id.  The terminal draws the image wherever
;; those characters land, so Emacs redisplay, scrolling and window
;; splits move the image like any other text.
;;
;; Image data is always sent inline (t=d), so it behaves the same
;; locally and when Emacs runs on a remote host over ssh.  Inside
;; tmux, escapes are wrapped for passthrough, which needs
;; `set -g allow-passthrough on' in tmux.conf.
;;
;; Usage:
;;   (tty-image-mode 1)          ; visit image files -> see the image
;;   M-x tty-image-insert-file   ; insert an image at point
;;
;; Spec: https://sw.kovidgoyal.net/kitty/graphics-protocol/

;;; Code:

(require 'cl-lib)

;;; Customization

(defgroup tty-image nil
  "Display images in terminal Emacs using the kitty graphics protocol."
  :group 'multimedia
  :prefix "tty-image-")

(defcustom tty-image-terminal-regexp (rx (or "ghostty" "kitty" "wezterm"))
  "Regexp matched case-insensitively against TERM and TERM_PROGRAM.
A match means the terminal is assumed to support the protocol."
  :type 'regexp)

(defcustom tty-image-force nil
  "Non-nil means assume every tty supports the kitty graphics protocol.
Useful when TERM was rewritten on the way to a remote host, for
example to xterm-256color because xterm-ghostty terminfo is missing."
  :type 'boolean)

(defcustom tty-image-cell-size '(10 . 20)
  "Approximate (WIDTH . HEIGHT) of a terminal cell in pixels.
Used to choose an image's natural size in cells; images larger than
the window are always scaled down to fit."
  :type '(cons integer integer))

;;; Protocol encoding

(defconst tty-image--placeholder #x10EEEE
  "Unicode placeholder character the terminal replaces with image cells.")

;; Emacs only composes combining marks onto characters in the "base"
;; category (see `composition-function-table': "\\c.\\c^+").  Private
;; use characters aren't in it, so without this the row diacritics
;; never reach the terminal attached to the placeholder and every
;; line shows the image's first row.
(modify-category-entry tty-image--placeholder ?.)

(defconst tty-image--diacritics
  [#x0305 #x030D #x030E #x0310 #x0312 #x033D #x033E #x033F #x0346 #x034A
   #x034B #x034C #x0350 #x0351 #x0352 #x0357 #x035B #x0363 #x0364 #x0365
   #x0366 #x0367 #x0368 #x0369 #x036A #x036B #x036C #x036D #x036E #x036F
   #x0483 #x0484 #x0485 #x0486 #x0487 #x0592 #x0593 #x0594 #x0595 #x0597
   #x0598 #x0599 #x059C #x059D #x059E #x059F #x05A0 #x05A1 #x05A8 #x05A9
   #x05AB #x05AC #x05AF #x05C4 #x0610 #x0611 #x0612 #x0613 #x0614 #x0615
   #x0616 #x0617 #x0657 #x0658 #x0659 #x065A #x065B #x065D #x065E #x06D6
   #x06D7 #x06D8 #x06D9 #x06DA #x06DB #x06DC #x06DF #x06E0 #x06E1 #x06E2
   #x06E4 #x06E7 #x06E8 #x06EB #x06EC #x0730 #x0732 #x0733 #x0735 #x0736
   #x073A #x073D #x073F #x0740 #x0741 #x0743 #x0745 #x0747 #x0749 #x074A
   #x07EB #x07EC #x07ED #x07EE #x07EF #x07F0 #x07F1 #x07F3 #x0816 #x0817
   #x0818 #x0819 #x081B #x081C #x081D #x081E #x081F #x0820 #x0821 #x0822
   #x0823 #x0825 #x0826 #x0827 #x0829 #x082A #x082B #x082C #x082D #x0951
   #x0953 #x0954 #x0F82 #x0F83 #x0F86 #x0F87 #x135D #x135E #x135F #x17DD
   #x193A #x1A17 #x1A75 #x1A76 #x1A77 #x1A78 #x1A79 #x1A7A #x1A7B #x1A7C
   #x1B6B #x1B6D #x1B6E #x1B6F #x1B70 #x1B71 #x1B72 #x1B73 #x1CD0 #x1CD1
   #x1CD2 #x1CDA #x1CDB #x1CE0 #x1DC0 #x1DC1 #x1DC3 #x1DC4 #x1DC5 #x1DC6
   #x1DC7 #x1DC8 #x1DC9 #x1DCB #x1DCC #x1DD1 #x1DD2 #x1DD3 #x1DD4 #x1DD5
   #x1DD6 #x1DD7 #x1DD8 #x1DD9 #x1DDA #x1DDB #x1DDC #x1DDD #x1DDE #x1DDF
   #x1DE0 #x1DE1 #x1DE2 #x1DE3 #x1DE4 #x1DE5 #x1DE6 #x1DFE #x20D0 #x20D1
   #x20D4 #x20D5 #x20D6 #x20D7 #x20DB #x20DC #x20E1 #x20E7 #x20E9 #x20F0
   #x2CEF #x2CF0 #x2CF1 #x2DE0 #x2DE1 #x2DE2 #x2DE3 #x2DE4 #x2DE5 #x2DE6
   #x2DE7 #x2DE8 #x2DE9 #x2DEA #x2DEB #x2DEC #x2DED #x2DEE #x2DEF #x2DF0
   #x2DF1 #x2DF2 #x2DF3 #x2DF4 #x2DF5 #x2DF6 #x2DF7 #x2DF8 #x2DF9 #x2DFA
   #x2DFB #x2DFC #x2DFD #x2DFE #x2DFF #xA66F #xA67C #xA67D #xA6F0 #xA6F1
   #xA8E0 #xA8E1 #xA8E2 #xA8E3 #xA8E4 #xA8E5 #xA8E6 #xA8E7 #xA8E8 #xA8E9
   #xA8EA #xA8EB #xA8EC #xA8ED #xA8EE #xA8EF #xA8F0 #xA8F1 #xAAB0 #xAAB2
   #xAAB3 #xAAB7 #xAAB8 #xAABE #xAABF #xAAC1 #xFE20 #xFE21 #xFE22 #xFE23
   #xFE24 #xFE25 #xFE26 #x10A0F #x10A38 #x1D185 #x1D186 #x1D187 #x1D188
   #x1D189 #x1D1AA #x1D1AB #x1D1AC #x1D1AD #x1D242 #x1D243 #x1D244]
  "Combining marks encoding row/column numbers 0, 1, 2, ...
From kitty's gen/rowcolumn-diacritics.txt.")

(defconst tty-image--chunk-size 4096
  "Maximum base64 payload bytes per escape code, as the spec requires.")

(defconst tty-image--png-signature
  (unibyte-string #x89 ?P ?N ?G #x0d #x0a #x1a #x0a))

(defun tty-image--apc (control &optional payload)
  "Return a graphics escape code with CONTROL data and optional PAYLOAD."
  (concat "\e_G" control (and payload (concat ";" payload)) "\e\\"))

(defun tty-image--wrap-for-tmux (seq)
  "Wrap SEQ in a tmux passthrough DCS, doubling its ESC characters."
  (concat "\ePtmux;" (string-replace "\e" "\e\e" seq) "\e\\"))

(defun tty-image--transmit-codes (id png cols rows)
  "Return escape codes sending PNG as image ID with a COLS x ROWS placement.
The placement is virtual (U=1): nothing is drawn until placeholder
characters for ID are printed.  Responses are suppressed (q=2) so
they never show up as keyboard input."
  (let* ((data (base64-encode-string png t))
         (chunks (cl-loop for start from 0 below (length data)
                          by tty-image--chunk-size
                          collect (substring data start
                                             (min (length data)
                                                  (+ start tty-image--chunk-size))))))
    (cl-loop for (chunk . more) on chunks
             for first = t then nil
             collect (tty-image--apc
                      (concat (if first
                                  (format "a=T,U=1,f=100,i=%d,p=1,c=%d,r=%d,"
                                          id cols rows)
                                "")
                              (format "q=2,m=%d" (if more 1 0)))
                      chunk))))

(defun tty-image--place-code (id cols rows)
  "Return an escape code replacing the virtual placement of ID with COLS x ROWS."
  (tty-image--apc (format "a=p,U=1,q=2,i=%d,p=1,c=%d,r=%d" id cols rows)))

(defun tty-image--delete-code (id)
  "Return an escape code deleting image ID and freeing its data."
  (tty-image--apc (format "a=d,d=I,q=2,i=%d" id)))

(defun tty-image--placeholder-text (cols rows)
  "Return ROWS lines of COLS placeholder characters.
Only the first cell of each line carries row/column diacritics; the
terminal infers the rest from the cell to the left."
  (when (> rows (length tty-image--diacritics))
    (error "tty-image: %d rows exceeds the protocol maximum of %d"
           rows (length tty-image--diacritics)))
  (mapconcat (lambda (row)
               (concat (string tty-image--placeholder
                               (aref tty-image--diacritics row)
                               (aref tty-image--diacritics 0))
                       (make-string (1- cols) tty-image--placeholder)))
             (number-sequence 0 (1- rows))
             "\n"))

;;; Image data and sizing

(defun tty-image--png-p (data)
  "Return non-nil if unibyte string DATA starts with the PNG signature."
  (string-prefix-p tty-image--png-signature data))

(defun tty-image--png-size (png)
  "Return (WIDTH . HEIGHT) in pixels read from PNG's IHDR chunk."
  (unless (and (tty-image--png-p png) (>= (length png) 24))
    (error "tty-image: data is not a PNG"))
  (cl-flet ((be32 (offset)
              (logior (ash (aref png offset) 24)
                      (ash (aref png (+ offset 1)) 16)
                      (ash (aref png (+ offset 2)) 8)
                      (aref png (+ offset 3)))))
    (cons (be32 16) (be32 20))))

(defun tty-image--fit (pixel-size max-cols max-rows)
  "Return (COLS . ROWS) for an image of PIXEL-SIZE (W . H).
The natural size comes from `tty-image-cell-size'; it is scaled
down, keeping the aspect ratio, to fit MAX-COLS x MAX-ROWS."
  (let* ((cols (/ (float (car pixel-size)) (car tty-image-cell-size)))
         (rows (/ (float (cdr pixel-size)) (cdr tty-image-cell-size)))
         (scale (min 1.0 (/ max-cols cols) (/ max-rows rows))))
    (cons (max 1 (round (* cols scale)))
          (max 1 (round (* rows scale))))))

(defun tty-image--converter (file png)
  "Return a command list converting FILE to PNG, or nil if none is installed."
  (cond ((executable-find "magick") (list "magick" file png))
        ((executable-find "convert") (list "convert" file png))
        ((executable-find "sips") (list "sips" "-s" "format" "png" file
                                        "--out" png))))

(defun tty-image--read-png (file)
  "Return FILE's contents as a unibyte PNG string, converting if needed."
  (let ((data (with-temp-buffer
                (set-buffer-multibyte nil)
                (insert-file-contents-literally file)
                (buffer-string))))
    (if (tty-image--png-p data)
        data
      (let* ((png (make-temp-file "tty-image" nil ".png"))
             (cmd (or (tty-image--converter (expand-file-name file) png)
                      (error "tty-image: %s is not a PNG and no converter \
\(magick, convert, sips) is installed" file))))
        (unwind-protect
            (let ((status (apply #'call-process (car cmd) nil nil nil (cdr cmd))))
              (unless (eq status 0)
                (error "tty-image: %s failed on %s (exit %s)" (car cmd) file status))
              (tty-image--read-png png))
          (delete-file png))))))

;;; Terminal

(defun tty-image--tmux-p (frame)
  "Return non-nil if FRAME's terminal is inside tmux."
  (getenv "TMUX" frame))

(defun tty-image--outer-terminal-names (frame)
  "Return names describing the terminal emulator behind FRAME.
Inside tmux, TERM describes tmux itself, so ask tmux for the client's."
  (append (list (getenv "TERM" frame) (getenv "TERM_PROGRAM" frame))
          (and (tty-image--tmux-p frame)
               (executable-find "tmux")
               (list (string-trim
                      (shell-command-to-string
                       "tmux display-message -p '#{client_termname}'"))))))

(defun tty-image-supported-p (&optional frame)
  "Return non-nil if FRAME is a tty that can show kitty protocol images."
  (and (not (display-graphic-p frame))
       (or tty-image-force
           (let ((case-fold-search t))
             (cl-some (lambda (name)
                        (and name (string-match-p tty-image-terminal-regexp name)))
                      (tty-image--outer-terminal-names frame))))))

(defun tty-image--send (code frame)
  "Write escape CODE to FRAME's terminal."
  (send-string-to-terminal (if (tty-image--tmux-p frame)
                               (tty-image--wrap-for-tmux code)
                             code)
                           (frame-terminal frame)))

(defun tty-image--truecolor-p (frame)
  "Return non-nil if FRAME's terminal shows 24-bit colors."
  (>= (display-color-cells frame) 16777216))

(defvar tty-image--live-ids (make-hash-table)
  "Image ids this Emacs has transmitted and not yet deleted.")

(defun tty-image--new-id (frame)
  "Return an unused image id encodable in FRAME's foreground colors.
With 24-bit color the id is the color itself.  With 256 colors it
is a palette index; 0-15 are skipped because Emacs emits those with
legacy SGR codes rather than 38;5;N."
  (let ((id nil))
    (while (or (null id) (gethash id tty-image--live-ids))
      (setq id (if (tty-image--truecolor-p frame)
                   (1+ (random #xfffffe))
                 (+ 16 (random 240)))))
    (puthash id t tty-image--live-ids)
    id))

(defun tty-image--id-color (id frame)
  "Return the color name whose value on FRAME encodes image ID."
  (if (tty-image--truecolor-p frame)
      (format "#%06x" id)
    (or (car (cl-find id (tty-color-alist frame) :key #'cadr))
        (error "tty-image: no tty color with index %d" id))))

;;; Public API

(defun tty-image-transmit (png cols rows &optional frame id)
  "Send PNG to FRAME's terminal for display over COLS x ROWS cells.
Reuse image ID if given, replacing its old data.  Return the id."
  (let* ((frame (or frame (selected-frame)))
         (id (or id (tty-image--new-id frame))))
    (dolist (code (tty-image--transmit-codes id png cols rows))
      (tty-image--send code frame))
    id))

(defun tty-image-delete (id &optional frame)
  "Delete image ID from FRAME's terminal."
  (tty-image--send (tty-image--delete-code id) (or frame (selected-frame)))
  (remhash id tty-image--live-ids))

(defun tty-image-placeholder-string (id cols rows &optional frame)
  "Return a string that shows image ID as COLS x ROWS cells on FRAME."
  (let ((color (tty-image--id-color id (or frame (selected-frame)))))
    (propertize (tty-image--placeholder-text cols rows)
                'face `(:foreground ,color)
                'font-lock-face `(:foreground ,color))))

(defun tty-image--window-fit (pixel-size window)
  "Return (COLS . ROWS) fitting PIXEL-SIZE in WINDOW's body.
One column is left free so lines never wrap or show `$'."
  (tty-image--fit pixel-size
                  (max 1 (1- (window-body-width window)))
                  (max 1 (window-body-height window))))

;;;###autoload
(defun tty-image-insert-file (file)
  "Insert the image in FILE at point, sized to fit the selected window."
  (interactive "fImage file: ")
  (unless (tty-image-supported-p)
    (user-error "This frame's terminal doesn't support the kitty graphics protocol"))
  (let* ((png (tty-image--read-png file))
         (size (tty-image--window-fit (tty-image--png-size png) (selected-window)))
         (id (tty-image-transmit png (car size) (cdr size))))
    (insert (tty-image-placeholder-string id (car size) (cdr size)))))

;;; Viewing image files

(defvar-local tty-image--view nil
  "Plist describing the image shown by `tty-image-view-mode'.
Keys: :id, :frame, :pixel-size, :cols, :rows, :overlay.")

(defun tty-image--view-show (cols rows)
  "Show the current buffer's image over COLS x ROWS cells."
  (let ((ov (plist-get tty-image--view :overlay)))
    (overlay-put ov 'display
                 (tty-image-placeholder-string (plist-get tty-image--view :id)
                                               cols rows
                                               (plist-get tty-image--view :frame)))
    (setq tty-image--view (plist-put (plist-put tty-image--view :cols cols)
                                     :rows rows))))

(defun tty-image-view-refresh ()
  "Re-send this buffer's image to the terminal and redraw it.
Useful after the terminal was reset or the file changed on disk."
  (interactive)
  (let* ((window (or (get-buffer-window nil t) (selected-window)))
         (frame (window-frame window))
         (png (tty-image--read-png buffer-file-name))
         (pixel-size (tty-image--png-size png))
         (size (tty-image--window-fit pixel-size window))
         (id (tty-image-transmit png (car size) (cdr size) frame
                                 (plist-get tty-image--view :id))))
    (setq tty-image--view
          (list :id id :frame frame :pixel-size pixel-size
                :overlay (or (plist-get tty-image--view :overlay)
                             (make-overlay (point-min) (point-max) nil nil t))))
    (tty-image--view-show (car size) (cdr size))))

(defun tty-image--view-resize (window)
  "Refit the image to WINDOW after its size or buffer changed."
  (with-current-buffer (window-buffer window)
    (when tty-image--view
      (let ((size (tty-image--window-fit (plist-get tty-image--view :pixel-size)
                                         window)))
        (unless (and (= (car size) (plist-get tty-image--view :cols))
                     (= (cdr size) (plist-get tty-image--view :rows)))
          (tty-image--send (tty-image--place-code (plist-get tty-image--view :id)
                                                  (car size) (cdr size))
                           (plist-get tty-image--view :frame))
          (tty-image--view-show (car size) (cdr size)))))))

(defun tty-image--view-cleanup ()
  "Free the terminal's copy of this buffer's image."
  (when tty-image--view
    (tty-image-delete (plist-get tty-image--view :id)
                      (plist-get tty-image--view :frame))))

(defvar-keymap tty-image-view-mode-map
  :parent special-mode-map
  "g" #'tty-image-view-refresh)

(define-derived-mode tty-image-view-mode special-mode "TTY-Image"
  "Major mode showing an image file in a kitty-protocol terminal.
The buffer keeps the file's bytes; an overlay displays placeholder
characters that the terminal replaces with the image."
  (setq truncate-lines t)
  (add-hook 'window-size-change-functions #'tty-image--view-resize nil t)
  (add-hook 'window-buffer-change-functions #'tty-image--view-resize nil t)
  (add-hook 'kill-buffer-hook #'tty-image--view-cleanup nil t)
  (add-hook 'change-major-mode-hook #'tty-image--view-cleanup nil t)
  (tty-image-view-refresh))

(defun tty-image--image-mode-advice (orig-fn &rest args)
  "Use `tty-image-view-mode' instead of ORIG-FN on capable ttys.
ARGS are passed to ORIG-FN otherwise."
  (if (and buffer-file-name (tty-image-supported-p))
      (tty-image-view-mode)
    (apply orig-fn args)))

;;;###autoload
(define-minor-mode tty-image-mode
  "Show image files as images when visited in a kitty-protocol tty.
GUI frames and other terminals keep the normal `image-mode'."
  :global t
  (if tty-image-mode
      (advice-add 'image-mode :around #'tty-image--image-mode-advice)
    (advice-remove 'image-mode #'tty-image--image-mode-advice)))

(provide 'tty-image)
;;; tty-image.el ends here
