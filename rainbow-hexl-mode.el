;;; rainbow-hexl-mode.el --- Colorize hex bytes in hexl-mode -*- lexical-binding: t -*-

;; Author: Claude & User
;; Version: 1.0
;; Keywords: data, hex, faces

;;; Commentary:

;; This minor mode colorizes hex bytes in hexl-mode buffers using a
;; rainbow gradient based on byte values. The colorization combines:
;;
;; 1. Brightness gradient: 00 (30% grey) → FF (white)
;; 2. Hue gradient: 0x (red) → Fx (purple/pink)
;;
;; Usage:
;;   M-x hexl-mode
;;   M-x rainbow-hexl-mode
;;
;; The mode can be toggled on/off and works efficiently with large
;; binary files using jit-lock lazy fontification.

;;; Code:

(require 'hexl)
(require 'cl-lib)

;;; Customization

(defgroup rainbow-hexl nil
  "Colorize hex bytes in hexl-mode buffers."
  :group 'hexl
  :group 'faces)

(defcustom rainbow-hexl-saturation 0.8
  "Saturation value for rainbow colors (0.0 to 1.0).
Higher values produce more vibrant colors."
  :type 'float
  :group 'rainbow-hexl)

(defcustom rainbow-hexl-min-lightness 0.3
  "Minimum lightness for byte value 0x00 (0.0 to 1.0).
Lower values produce darker colors for low byte values."
  :type 'float
  :group 'rainbow-hexl)

(defcustom rainbow-hexl-max-lightness 1.0
  "Maximum lightness for byte value 0xFF (0.0 to 1.0).
Higher values produce brighter colors for high byte values."
  :type 'float
  :group 'rainbow-hexl)

;;; Color Conversion Functions

(defun rainbow-hexl--hsl-to-rgb (h s l)
  "Convert HSL color to RGB hex string.
H is hue in degrees (0-360).
S is saturation (0.0-1.0).
L is lightness (0.0-1.0).
Returns RGB color as hex string #RRGGBB."
  (let* ((h-norm (/ (mod h 360) 360.0))
         (c (* (- 1.0 (abs (- (* 2.0 l) 1.0))) s))
         (x (* c (- 1.0 (abs (- (mod (* h-norm 6.0) 2.0) 1.0)))))
         (m (- l (* 0.5 c)))
         (rgb-prime
          (cond
           ((< h-norm (/ 1.0 6.0)) (list c x 0.0))
           ((< h-norm (/ 2.0 6.0)) (list x c 0.0))
           ((< h-norm (/ 3.0 6.0)) (list 0.0 c x))
           ((< h-norm (/ 4.0 6.0)) (list 0.0 x c))
           ((< h-norm (/ 5.0 6.0)) (list x 0.0 c))
           (t (list c 0.0 x))))
         (r (round (* 255 (+ (nth 0 rgb-prime) m))))
         (g (round (* 255 (+ (nth 1 rgb-prime) m))))
         (b (round (* 255 (+ (nth 2 rgb-prime) m)))))
    (format "#%02x%02x%02x" r g b)))

(defun rainbow-hexl--byte-to-color (byte)
  "Convert BYTE (0-255) to RGB color string.
Uses hue based on first nibble (0-F) and lightness based on full byte value.
Returns color as hex string #RRGGBB."
  (let* ((nibble (ash byte -4))
         (hue (* nibble (/ 360.0 16.0)))
         (lightness (+ rainbow-hexl-min-lightness
                      (* (/ byte 255.0)
                         (- rainbow-hexl-max-lightness
                            rainbow-hexl-min-lightness)))))
    (rainbow-hexl--hsl-to-rgb hue rainbow-hexl-saturation lightness)))

;;; Face Management

(defvar rainbow-hexl--faces (make-vector 256 nil)
  "Vector of 256 faces for hex byte colorization.
Each element is a face symbol for the corresponding byte value (0x00-0xFF).")

(defvar rainbow-hexl--faces-initialized nil
  "Non-nil if rainbow-hexl faces have been initialized.")

(defun rainbow-hexl--initialize-faces ()
  "Create all 256 faces for hex byte colorization.
Only initializes once; subsequent calls are no-ops."
  (unless rainbow-hexl--faces-initialized
    (dotimes (byte 256)
      (let* ((face-symbol (intern (format "rainbow-hexl-byte-%02x" byte)))
             (color (rainbow-hexl--byte-to-color byte)))
        (make-face face-symbol)
        (set-face-foreground face-symbol color)
        (aset rainbow-hexl--faces byte face-symbol)))
    (setq rainbow-hexl--faces-initialized t)))

(defun rainbow-hexl--reinitialize-faces ()
  "Reinitialize all faces with current customization values.
Useful after changing `rainbow-hexl-saturation' or lightness settings."
  (interactive)
  (dotimes (byte 256)
    (let* ((face-symbol (aref rainbow-hexl--faces byte))
           (color (rainbow-hexl--byte-to-color byte)))
      (when face-symbol
        (set-face-foreground face-symbol color))))
  (when (and (boundp 'rainbow-hexl-mode) rainbow-hexl-mode)
    (font-lock-flush)))

(defun rainbow-hexl-refontify ()
  "Manually refontify the entire buffer.
Use this if colors don't update correctly after editing."
  (interactive)
  (when rainbow-hexl-mode
    (rainbow-hexl--clear-all-overlays)
    (jit-lock-refontify)))

;;; Fontification Engine

(defvar-local rainbow-hexl--overlays nil
  "List of overlays used for colorization in the current buffer.")

(defun rainbow-hexl--after-change (beg end _old-len)
  "Update colorization after buffer modification.
BEG and END mark the changed region, _OLD-LEN is ignored."
  (when rainbow-hexl-mode
    ;; Remove overlays in the changed region
    (rainbow-hexl--unfontify-region (save-excursion (goto-char beg) (line-beginning-position))
                                    (save-excursion (goto-char end) (line-end-position)))
    ;; Re-fontify the region (jit-lock will call our function)
    (jit-lock-refontify (save-excursion (goto-char beg) (line-beginning-position))
                        (save-excursion (goto-char end) (line-end-position)))))

(defun rainbow-hexl--fontify-region (start end)
  "Fontify hex bytes in region from START to END using overlays.
Parses hexl-mode format and applies rainbow colorization to hex bytes."
  (save-excursion
    (goto-char start)
    (beginning-of-line)
    (while (< (point) end)
      ;; Check if this line looks like a hexl-mode line
      (when (looking-at "^[0-9a-f]+: ")
        (goto-char (match-end 0))
        (let* ((line-end (line-end-position))
               ;; Find the ASCII region (marked by "  " - two spaces)
               (ascii-start (save-excursion
                             (if (re-search-forward "  [^ ]" line-end t)
                                 (- (point) 1)
                               line-end)))
               (hex-region-end (max (point) (- ascii-start 2))))
          ;; Fontify hex bytes in the hex region
          (while (and (< (point) hex-region-end)
                      (re-search-forward "\\([0-9a-f]\\{2\\}\\)" hex-region-end t))
            (let* ((byte-start (match-beginning 1))
                   (byte-end (match-end 1))
                   (hex-str (match-string 1))
                   (byte (string-to-number hex-str 16))
                   (face (aref rainbow-hexl--faces byte)))
              (when face
                ;; Check if there's already an overlay at this position
                (let ((existing-ov (cl-find-if
                                   (lambda (ov)
                                     (and (overlay-get ov 'rainbow-hexl)
                                          (= (overlay-start ov) byte-start)
                                          (= (overlay-end ov) byte-end)))
                                   (overlays-at byte-start))))
                  (if existing-ov
                      ;; Update existing overlay
                      (overlay-put existing-ov 'face face)
                    ;; Create new overlay
                    (let ((ov (make-overlay byte-start byte-end)))
                      (overlay-put ov 'face face)
                      (overlay-put ov 'rainbow-hexl t)
                      (overlay-put ov 'evaporate t)  ; Auto-delete on text deletion
                      (push ov rainbow-hexl--overlays)))))))))
      (forward-line 1)))
  ;; jit-lock expects us to return non-nil on success
  'fontified)

(defun rainbow-hexl--unfontify-region (start end)
  "Remove rainbow-hexl fontification from region START to END."
  (dolist (ov (overlays-in start end))
    (when (overlay-get ov 'rainbow-hexl)
      (delete-overlay ov)
      (setq rainbow-hexl--overlays (delq ov rainbow-hexl--overlays)))))

(defun rainbow-hexl--clear-all-overlays ()
  "Remove all rainbow-hexl overlays from the current buffer."
  (dolist (ov rainbow-hexl--overlays)
    (delete-overlay ov))
  (setq rainbow-hexl--overlays nil))

;;; Minor Mode Definition

;;;###autoload
(define-minor-mode rainbow-hexl-mode
  "Toggle rainbow colorization of hex bytes in hexl-mode.

When enabled, hex bytes are colorized based on their values:
- Brightness: 00 (dark grey) → FF (bright white)
- Hue: 0x (red) → Fx (purple/pink)

This mode uses overlays and jit-lock for efficient lazy fontification,
making it suitable for large binary files.

\\{rainbow-hexl-mode-map}"
  :lighter " Rainbow"
  :group 'rainbow-hexl
  (cond
   (rainbow-hexl-mode
    ;; Enabling mode
    (unless (eq major-mode 'hexl-mode)
      (setq rainbow-hexl-mode nil)
      (user-error "Rainbow-hexl-mode requires hexl-mode"))
    ;; Initialize faces if needed
    (rainbow-hexl--initialize-faces)
    ;; Clear any existing overlays
    (rainbow-hexl--clear-all-overlays)
    ;; Register fontification with jit-lock
    (jit-lock-register #'rainbow-hexl--fontify-region)
    ;; Install after-change hook to handle edits
    (add-hook 'after-change-functions #'rainbow-hexl--after-change nil t)
    ;; Force immediate fontification of visible area
    (jit-lock-refontify))
   (t
    ;; Disabling mode
    (jit-lock-unregister #'rainbow-hexl--fontify-region)
    ;; Remove after-change hook
    (remove-hook 'after-change-functions #'rainbow-hexl--after-change t)
    ;; Remove all overlays
    (rainbow-hexl--clear-all-overlays))))

(provide 'rainbow-hexl-mode)

;;; rainbow-hexl-mode.el ends here
