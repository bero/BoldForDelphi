#!/bin/sh
# Builds and runs the Invoicing scenario runner and the FPCUnit tests under
# Free Pascal (Linux, FPC 3.2.2+).
# Run from anywhere; output goes to examples/Compound/Invoicing/FPC/bin.
# Exit code 0 when every check and every test passed.
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

# shellcheck disable=SC2086
fpc -Mdelphiunicode -vew \
  -Fi"$ROOT/Source/Common/Include" $UNITPATHS \
  -Fi"$INV/Model" -Fu"$INV/Model" -Fu"$INV/Tests" \
  -FE"$OUT" -FU"$OUT" \
  "$HERE/InvoicingFPCTests.lpr"

set +e
"$OUT/InvoicingFPC" "$INV/Model/Invoicing.bld"
RUNNER=$?
"$OUT/InvoicingFPCTests" --all --format=plain
TESTS=$?
[ $RUNNER -eq 0 ] && [ $TESTS -eq 0 ]
