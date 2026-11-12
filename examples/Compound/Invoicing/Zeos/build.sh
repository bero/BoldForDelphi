#!/bin/sh
# Builds the Invoicing scenario runner on SQLite through Zeos with lazbuild and
# runs it (Linux, Lazarus 4 with FPC 3.2.2+).
# Requires Zeos 8.0: set ZEOS to its root folder, the one that contains
# packages/lazarus (see UnitTest/README.md). The Zeos packages are made known
# to Lazarus here; building them is lazbuild's job.
# Run from anywhere; the program is examples/Compound/Invoicing/Zeos/bin/InvoicingZeos.
# Exit code 0 when the build and every check passed.
#
# LAZBUILD_OPTIONS adds lazbuild options, e.g. --pcp=<dir> for a Lazarus
# configuration of its own.
set -e
HERE=$(cd "$(dirname "$0")" && pwd)
if [ -z "$ZEOS" ] || [ ! -f "$ZEOS/packages/lazarus/zcomponent.lpk" ]; then
  echo "build.sh: Zeos 8.0 is required for this runner." >&2
  echo "Set ZEOS to the Zeos root folder (the one that contains packages/lazarus)," >&2
  echo "e.g. export ZEOS=~/components/zeosdbo-8.0.0. See UnitTest/README.md." >&2
  exit 1
fi

for package in zcore zplain zparsesql zdbc zcomponent; do
  # shellcheck disable=SC2086
  lazbuild $LAZBUILD_OPTIONS --add-package-link "$ZEOS/packages/lazarus/$package.lpk"
done
# shellcheck disable=SC2086
lazbuild $LAZBUILD_OPTIONS "$HERE/InvoicingZeos.lpi"

"$HERE/bin/InvoicingZeos"
