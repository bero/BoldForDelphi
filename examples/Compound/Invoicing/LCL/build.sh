#!/bin/sh
# Builds the LCL client of the Invoicing example with lazbuild and runs its
# self-test (Linux, Lazarus 4 with FPC 3.2.2+).
# Run from anywhere; the program is examples/Compound/Invoicing/LCL/bin/InvoicingLCL.
# Exit code 0 when the build and every self-test check passed.
#
# LCL_WS chooses the widgetset (default qt5; gtk2 needs the GTK 2 development
# libraries). The self-test shows no window, but the widgetset still needs a
# display to start.
set -e
HERE=$(cd "$(dirname "$0")" && pwd)
lazbuild --ws="${LCL_WS:-qt5}" "$HERE/InvoicingLCL.lpi"

set +e
"$HERE/bin/InvoicingLCL" --selftest
RESULT=$?
cat "$HERE/bin/InvoicingLCL.selftest.txt"
exit $RESULT
