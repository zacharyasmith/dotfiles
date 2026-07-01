#!/bin/bash
# Interactive test for rainbow-hexl-mode

# Create a test file
python3 -c "import sys; sys.stdout.buffer.write(bytes(range(256)))" > /tmp/rainbow-test-colors.bin

echo "Starting Emacs with rainbow-hexl-mode test..."
echo ""
echo "Instructions:"
echo "1. The file /tmp/rainbow-test-colors.bin is now open"
echo "2. Press M-x and type: hexl-mode"
echo "3. Press M-x and type: rainbow-hexl-mode"
echo "4. You should see colored hex bytes!"
echo ""
echo "Expected colors:"
echo "  - 00-0F = Red shades (dark to brighter)"
echo "  - 50-5F = Green shades"
echo "  - 80-8F = Cyan shades"
echo "  - F0-FF = Pink/Purple shades (brightest)"
echo ""

# Start Emacs with the test file
emacs /tmp/rainbow-test-colors.bin
