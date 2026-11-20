#!/bin/sh
# Builds the LCL client of the standalone model editor (#128) with lazbuild
# and runs its self-test on the Invoicing example's model (Linux, or Git Bash
# on Windows; Lazarus 4 with FPC 3.2.2+).
# Run from anywhere; the program is Tools/ModelEditor/LCL/bin/ModelEditorLCL.
# Exit code 0 when the build and every self-test check passed.
#
# LCL_WS chooses the widgetset (default qt5, win32 on Windows; gtk2 needs the
# GTK 2 development libraries). The self-test shows no window, but the
# widgetset still needs a display to start; without one,
# QT_QPA_PLATFORM=offscreen does for Qt5.
# LAZBUILD_OPTIONS adds lazbuild options, e.g. --pcp=<dir> for a Lazarus
# configuration of its own.
set -e
HERE=$(cd "$(dirname "$0")" && pwd)
ROOT=$(cd "$HERE/../../.." && pwd)
# Windows paths and programs under Git Bash; nothing on Linux
. "$ROOT/Tools/fpc-env.sh"
HERE=$(bold_path "$HERE")
ROOT=$(bold_path "$ROOT")
# shellcheck disable=SC2086
lazbuild --ws="${LCL_WS:-qt5}" $LAZBUILD_OPTIONS "$HERE/ModelEditorLCL.lpi"

set +e
"$HERE/bin/ModelEditorLCL" --selftest "$ROOT/examples/Compound/Invoicing/Model/Invoicing.bld"
RESULT=$?
cat "$HERE/bin/ModelEditorLCL.selftest.txt"
exit $RESULT
