#!/bin/sh
# Builds and runs the Invoicing scenario under Free Pascal (Linux, FPC 3.2.2+).
# Run from anywhere; output goes to examples/Compound/Invoicing/FPC/bin.
# Exit code = number of failed checks (255 on an unexpected exception).
set -e
HERE=$(cd "$(dirname "$0")" && pwd)
ROOT=$(cd "$HERE/../../../.." && pwd)
INV="$ROOT/examples/Compound/Invoicing"
OUT="$HERE/bin"
mkdir -p "$OUT"

UNITPATHS=$(find "$ROOT/Source" -type d \
  -not -path '*/Deprecated*' -not -path '*/BoldAwareGUI*' -not -path '*/Unassigned*' \
  | sed 's/^/-Fu/')

# shellcheck disable=SC2086
fpc -Mdelphiunicode -vew \
  -Fi"$ROOT/Source/Common/Include" $UNITPATHS \
  -Fi"$INV/Model" -Fu"$INV/Model" -Fu"$INV/Tests" \
  -FE"$OUT" -FU"$OUT" \
  "$HERE/InvoicingFPC.lpr"

set +e
"$OUT/InvoicingFPC" "$INV/Model/Invoicing.bld"
