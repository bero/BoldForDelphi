# Sourced by the Free Pascal and Lazarus build scripts: UnitTest/FPC/build.sh,
# examples/Compound/Invoicing/{FPC,LCL,Zeos}/build.sh and
# packages/Lazarus/tests/build.sh, build-zeos.sh.
#
# On Linux it does nothing: bold_path returns its argument unchanged.
#
# In Git Bash on Windows (also MSYS2 and Cygwin) it lets the same scripts run
# the Windows programs fpc.exe and lazbuild.exe:
# - bold_path gives a path in the form C:/..., which Windows programs read. The
#   scripts pass HERE, ROOT and ZEOS through it, so every path they build from
#   them, also each folder that find lists for the unit path, has that form.
# - MSYS_NO_PATHCONV=1 (MSYS2_ARG_CONV_EXCL for MSYS2) keeps Git Bash from
#   rewriting the arguments for a Windows program: it does not convert a path
#   after an option such as -Fu, and it reads the ':' in -FEc:/... as the
#   separator of a path list.
# - fpc and lazbuild are taken from the PATH, else from the Lazarus folder,
#   LAZARUS_DIR (default C:/lazarus), and its fpc/<version>/bin/<target>.
# - LCL_WS, the widgetset of the LCL builds, defaults to win32 (the scripts
#   default it to qt5).
# POSIX sh only: the CI runs the scripts with dash.

BOLD_WINDOWS=
case "$(uname -s)" in
  MINGW*|MSYS*|CYGWIN*) BOLD_WINDOWS=1 ;;
esac

bold_path() {
  if [ -n "$BOLD_WINDOWS" ]; then
    cygpath -m "$1"
  else
    printf '%s\n' "$1"
  fi
}

if [ -n "$BOLD_WINDOWS" ]; then
  if ! command -v cygpath >/dev/null 2>&1; then
    echo "fpc-env.sh: cygpath not found; run the script in Git Bash or MSYS2." >&2
    exit 1
  fi
  MSYS_NO_PATHCONV=1
  MSYS2_ARG_CONV_EXCL='*'
  export MSYS_NO_PATHCONV MSYS2_ARG_CONV_EXCL
  LCL_WS=${LCL_WS:-win32}
  BOLD_LAZARUS_DIR=$(cygpath -u "${LAZARUS_DIR:-C:/lazarus}")
  if ! command -v lazbuild >/dev/null 2>&1 && [ -f "$BOLD_LAZARUS_DIR/lazbuild.exe" ]; then
    PATH="$BOLD_LAZARUS_DIR:$PATH"
  fi
  if ! command -v fpc >/dev/null 2>&1; then
    for BOLD_FPC_BIN in "$BOLD_LAZARUS_DIR"/fpc/*/bin/x86_64-win64 "$BOLD_LAZARUS_DIR"/fpc/*/bin/i386-win32; do
      if [ -f "$BOLD_FPC_BIN/fpc.exe" ]; then
        PATH="$BOLD_FPC_BIN:$PATH"
        break
      fi
    done
  fi
  if ! command -v fpc >/dev/null 2>&1; then
    echo "fpc-env.sh: Free Pascal (fpc) not found on the PATH or in ${LAZARUS_DIR:-C:/lazarus}." >&2
    echo "Install Lazarus, which includes Free Pascal, or set LAZARUS_DIR to its folder." >&2
    exit 1
  fi
  export PATH
fi
