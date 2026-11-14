#!/bin/sh
# Builds the Lazarus design-time package dclBoldZeos with lazbuild, then
# builds and runs BoldZeosRegistrationCheck, which checks without the IDE what
# the package registers (Linux, or Git Bash on Windows; Lazarus 4 with
# FPC 3.2.2+).
# Requires Zeos 8.0: set ZEOS to its root folder, the one that contains
# packages/lazarus (see UnitTest/README.md). The Zeos packages are made known
# to Lazarus here; building them is lazbuild's job.
# Run from anywhere. Exit code 0 when both builds and every check passed.
#
# dclBoldZeos requires dclBold, whose property editor needs a widgetset:
# LCL_WS, qt5 by default, win32 on Windows. The check program asks the IDE's
# interface which packages a project gets with the component, so it links the
# widgetset too; without a display, set QT_QPA_PLATFORM=offscreen.
#
# LAZBUILD_OPTIONS adds lazbuild options, e.g. --pcp=<dir> for a Lazarus
# configuration of its own.
set -e
HERE=$(cd "$(dirname "$0")" && pwd)
# Windows paths and programs under Git Bash; nothing on Linux
. "$HERE/../../../Tools/fpc-env.sh"
HERE=$(bold_path "$HERE")
if [ -n "$ZEOS" ]; then
  ZEOS=$(bold_path "$ZEOS")
fi
if [ -z "$ZEOS" ] || [ ! -f "$ZEOS/packages/lazarus/zcomponent.lpk" ]; then
  echo "build-zeos.sh: Zeos 8.0 is required for this check." >&2
  echo "Set ZEOS to the Zeos root folder (the one that contains packages/lazarus)," >&2
  echo "e.g. export ZEOS=~/components/zeosdbo-8.0.0. See UnitTest/README.md." >&2
  exit 1
fi

for package in zcore zplain zparsesql zdbc zcomponent; do
  # shellcheck disable=SC2086
  lazbuild $LAZBUILD_OPTIONS --add-package-link "$ZEOS/packages/lazarus/$package.lpk"
done
# shellcheck disable=SC2086
lazbuild --ws="${LCL_WS:-qt5}" $LAZBUILD_OPTIONS "$HERE/../dclBoldZeos.lpk"
# shellcheck disable=SC2086
lazbuild --ws="${LCL_WS:-qt5}" $LAZBUILD_OPTIONS "$HERE/BoldZeosRegistrationCheck.lpi"

"$HERE/bin/BoldZeosRegistrationCheck"
