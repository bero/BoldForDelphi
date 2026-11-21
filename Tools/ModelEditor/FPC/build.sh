#!/bin/sh
# Builds and runs the tests of the standalone model editor (#128) under Free
# Pascal (Linux, or Git Bash on Windows; FPC 3.2.2+).
# Run from anywhere; output goes to Tools/ModelEditor/FPC/bin.
# Arguments are passed to the test program (default: --all --format=plain).
# Exit code 0 when every test passed.
set -e
HERE=$(cd "$(dirname "$0")" && pwd)
ROOT=$(cd "$HERE/../../.." && pwd)
# Windows paths and programs under Git Bash; nothing on Linux
. "$ROOT/Tools/fpc-env.sh"
HERE=$(bold_path "$HERE")
ROOT=$(bold_path "$ROOT")
EDITOR="$ROOT/Tools/ModelEditor"
OUT="$HERE/bin"
mkdir -p "$OUT"

# As in UnitTest/FPC/build.sh: of BoldAwareGUI only the control packs, BoldGUI
# and TBoldDataSet build without the VCL; the shared data module of the
# clients (Core/BoldModelEditorData) needs TBoldDataSet.
UNITPATHS=$( { find "$ROOT/Source" -type d \
    -not -path '*/Deprecated*' -not -path '*/BoldAwareGUI*' -not -path '*/Unassigned*' \
    -not -path '*/EnvironmentLCL*' -not -path '*/IDELazarus*'; \
  echo "$ROOT/Source/BoldAwareGUI/ControlPacks"; \
  echo "$ROOT/Source/BoldAwareGUI/Core"; \
  echo "$ROOT/Source/BoldAwareGUI/BoldControls"; } | sed 's/^/-Fu/')

# shellcheck disable=SC2086
# -gl: line information, so that the call stacks of exceptions name their
# routines (#130)
fpc -Mdelphiunicode -vew -gl \
  -Fi"$ROOT/Source/Common/Include" $UNITPATHS \
  -Fu"$ROOT/UnitTest/Code/Common" \
  -Fu"$EDITOR/Core" -Fu"$EDITOR/Tests" \
  -FE"$OUT" -FU"$OUT" \
  "$HERE/ModelEditorTests.lpr"

set +e
if [ $# -eq 0 ]; then
  "$OUT/ModelEditorTests" --all --format=plain
else
  "$OUT/ModelEditorTests" "$@"
fi
