#!/bin/bash
# Interactive test for tty-image.el. Run inside Ghostty, locally or
# in an ssh session.

dir=$(mktemp -d)

# A 200x100 PNG: red left half, blue right half
python3 -I - "$dir/test.png" <<'EOF'
import struct, sys, zlib
w, h = 200, 100
row = b"\x00" + bytes([255, 0, 0]) * (w // 2) + bytes([0, 0, 255]) * (w // 2)
def chunk(t, d):
    return struct.pack(">I", len(d)) + t + d + struct.pack(">I", zlib.crc32(t + d))
open(sys.argv[1], "wb").write(
    b"\x89PNG\r\n\x1a\n"
    + chunk(b"IHDR", struct.pack(">IIBBBBB", w, h, 8, 2, 0, 0, 0))
    + chunk(b"IDAT", zlib.compress(row * h))
    + chunk(b"IEND", b""))
EOF

echo "Step 1: raw terminal check (no Emacs)."
echo "You should see a red/blue box below. If not, the terminal itself"
echo "doesn't support Unicode placeholders."
# macOS /bin/bash 3.2 printf has no \U escapes, so print via python.
python3 -I - "$dir/test.png" <<'EOF2'
import base64, sys
b64 = base64.b64encode(open(sys.argv[1], "rb").read()).decode()
out = sys.stdout
out.write(f"\033_Ga=T,U=1,f=100,i=42,c=10,r=2,q=2;{b64}\033\\")
for row in ("\u0305", "\u030D"):
    out.write("\033[38;5;42m\U0010EEEE" + row + "\u0305" + "\U0010EEEE" * 9 + "\033[39m\n")
out.flush()
EOF2
read -r -p "Press Enter to continue..."
printf '\033_Ga=d,d=I,i=42,q=2\033\\'
echo ""
read -r -p "Press Enter to open the image in emacs -nw (C-x C-c to quit)..."

# -Q keeps the test independent of ~/.emacs. If nothing shows over
# ssh, TERM was probably rewritten: add (setq tty-image-force t).
emacs -nw -Q -L "$(dirname "$0")" \
      --eval '(progn (require (quote tty-image)) (tty-image-mode 1))' \
      "$dir/test.png"
