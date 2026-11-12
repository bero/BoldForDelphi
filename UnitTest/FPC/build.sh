#!/bin/sh
# Builds and runs the Bold unit tests under Free Pascal (Linux, FPC 3.2.2+).
# Requires Zeos 8.0: set ZEOS to its root folder (see UnitTest/README.md).
# Arguments are passed to the test runner (default: all tests, plain output).
# Output goes to UnitTest/FPC/bin. Exit code 0 when every test passed.
#
# BOLD_HEAPTRC=1 builds with heaptrc (-gh) and line info (-gl) into
# UnitTest/FPC/bin-heaptrc instead, and also fails when the run leaves memory
# unfreed; heaptrc's report, with the line that allocated each block, is
# printed then. heaptrc also overwrites freed memory, so a use of freed memory
# raises instead of passing by luck.
set -e
HERE=$(cd "$(dirname "$0")" && pwd)
ROOT=$(cd "$HERE/../.." && pwd)
OUT="$HERE/bin"
HEAPTRC_OPTIONS=""
if [ "$BOLD_HEAPTRC" = "1" ]; then
  OUT="$HERE/bin-heaptrc"
  HEAPTRC_OPTIONS="-gh -gl"
fi
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
ZOUT="$HERE/bin/zeos"
mkdir -p "$ZOUT"
ZSRC="$ZEOS/src"
for unit in component/ZConnection component/ZDataset component/ZSqlProcessor dbc/ZDbcSqLite; do
  fpc -Mdelphi -ve \
    -Fi"$ZSRC" -Fu"$ZSRC/core" -Fu"$ZSRC/plain" -Fu"$ZSRC/parsesql" \
    -Fu"$ZSRC/dbc" -Fu"$ZSRC/component" \
    -FU"$ZOUT" "$ZSRC/$unit.pas"
done

# Source/Common/EnvironmentLCL needs the LCL, which this build does not have:
# leaving it off the path also proves that nothing in the core uses it. The
# same goes for Source/Common/IDELazarus, the design-time registration.
# BoldAwareGUI holds the VCL controls; of it only the control packs, BoldGUI and
# TBoldDataSet build without the VCL (BOLD_VCL in bold.inc), so only their
# folders are on the path.
UNITPATHS=$( { find "$ROOT/Source" -type d \
    -not -path '*/Deprecated*' -not -path '*/BoldAwareGUI*' -not -path '*/Unassigned*' \
    -not -path '*/EnvironmentLCL*' -not -path '*/IDELazarus*'; \
  echo "$ROOT/Source/BoldAwareGUI/ControlPacks"; \
  echo "$ROOT/Source/BoldAwareGUI/Core"; \
  echo "$ROOT/Source/BoldAwareGUI/BoldControls"; \
  find "$ROOT/UnitTest/Code" -type d; } | sed 's/^/-Fu/')

# shellcheck disable=SC2086
fpc -Mdelphiunicode -vew $HEAPTRC_OPTIONS \
  -Fi"$ROOT/Source/Common/Include" $UNITPATHS -Fu"$ZOUT" \
  -FE"$OUT" -FU"$OUT" \
  "$HERE/UnitTestFPC.lpr"

set +e
if [ "$BOLD_HEAPTRC" = "1" ]; then
  HEAPTRC_LOG="$OUT/heaptrc.log"
  rm -f "$HEAPTRC_LOG"
  HEAPTRC="log=$HEAPTRC_LOG"
  export HEAPTRC
fi
if [ $# -eq 0 ]; then
  "$OUT/UnitTestFPC" --all --format=plain
else
  "$OUT/UnitTestFPC" "$@"
fi
RESULT=$?
if [ "$BOLD_HEAPTRC" = "1" ] && ! grep -q '^0 unfreed memory blocks' "$HEAPTRC_LOG"; then
  cat "$HEAPTRC_LOG"
  echo "build.sh: the run left memory unfreed (heaptrc report above)" >&2
  [ $RESULT -eq 0 ] && RESULT=1
fi
exit $RESULT
