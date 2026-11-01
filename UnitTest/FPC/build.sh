#!/bin/sh
# Builds and runs the Bold unit tests under Free Pascal (Linux, FPC 3.2.2+).
# Requires Zeos 8.0: set ZEOS to its root folder (see UnitTest/README.md).
# Arguments are passed to the test runner (default: all tests, plain output).
# Output goes to UnitTest/FPC/bin. Exit code 0 when every test passed.
set -e
HERE=$(cd "$(dirname "$0")" && pwd)
ROOT=$(cd "$HERE/../.." && pwd)
OUT="$HERE/bin"
mkdir -p "$OUT"

# Zeos 8.0 (https://sourceforge.net/projects/zeoslib) is required: under Free
# Pascal, Bold's database tests use the Zeos adapter (Delphi uses FireDAC).
# ZEOS points to the Zeos root folder, the one that contains src/.
if [ -z "$ZEOS" ] || [ ! -f "$ZEOS/src/component/ZConnection.pas" ]; then
  echo "build.sh: Zeos 8.0 is required for the Free Pascal tests." >&2
  echo "Set ZEOS to the Zeos root folder (the one that contains src/)," >&2
  echo "e.g. export ZEOS=~/components/zeosdbo-8.0.0. See UnitTest/README.md." >&2
  exit 1
fi

# Zeos is compiled the way its Lazarus packages do it, in mode Delphi (String =
# AnsiString), into its own folder. -Mdelphiunicode below would also define
# UNICODE, which Zeos reads as "String is UnicodeString", and it does not
# compile that way. The Bold build then uses only these precompiled units.
ZOUT="$OUT/zeos"
mkdir -p "$ZOUT"
ZSRC="$ZEOS/src"
for unit in component/ZConnection component/ZDataset component/ZSqlProcessor dbc/ZDbcSqLite; do
  fpc -Mdelphi -ve \
    -Fi"$ZSRC" -Fu"$ZSRC/core" -Fu"$ZSRC/plain" -Fu"$ZSRC/parsesql" \
    -Fu"$ZSRC/dbc" -Fu"$ZSRC/component" \
    -FU"$ZOUT" "$ZSRC/$unit.pas"
done

UNITPATHS=$( { find "$ROOT/Source" -type d \
    -not -path '*/Deprecated*' -not -path '*/BoldAwareGUI*' -not -path '*/Unassigned*'; \
  find "$ROOT/UnitTest/Code" -type d; } | sed 's/^/-Fu/')

# shellcheck disable=SC2086
fpc -Mdelphiunicode -vew \
  -Fi"$ROOT/Source/Common/Include" $UNITPATHS -Fu"$ZOUT" \
  -FE"$OUT" -FU"$OUT" \
  "$HERE/UnitTestFPC.lpr"

set +e
if [ $# -eq 0 ]; then
  "$OUT/UnitTestFPC" --all --format=plain
else
  "$OUT/UnitTestFPC" "$@"
fi
