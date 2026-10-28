#!/bin/sh
# Builds and runs the Bold unit tests under Free Pascal (Linux, FPC 3.2.2+).
# Arguments are passed to the test runner (default: all tests, plain output).
# Output goes to UnitTest/FPC/bin. Exit code 0 when every test passed.
set -e
HERE=$(cd "$(dirname "$0")" && pwd)
ROOT=$(cd "$HERE/../.." && pwd)
OUT="$HERE/bin"
mkdir -p "$OUT"

UNITPATHS=$( { find "$ROOT/Source" -type d \
    -not -path '*/Deprecated*' -not -path '*/BoldAwareGUI*' -not -path '*/Unassigned*'; \
  find "$ROOT/UnitTest/Code" -type d; } | sed 's/^/-Fu/')

# shellcheck disable=SC2086
fpc -Mdelphiunicode -vew \
  -Fi"$ROOT/Source/Common/Include" $UNITPATHS \
  -FE"$OUT" -FU"$OUT" \
  "$HERE/UnitTestFPC.lpr"

set +e
if [ $# -eq 0 ]; then
  "$OUT/UnitTestFPC" --all --format=plain
else
  "$OUT/UnitTestFPC" "$@"
fi
