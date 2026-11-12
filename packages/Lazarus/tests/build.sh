#!/bin/sh
# Builds the Lazarus design-time package dclBold with lazbuild, then builds
# and runs BoldRegistrationCheck, which checks without the IDE what the
# package registers (Linux, Lazarus 4 with FPC 3.2.2+).
# Run from anywhere. Exit code 0 when both builds and every check passed.
#
# LAZBUILD_OPTIONS adds lazbuild options, e.g. --pcp=<dir> for a Lazarus
# configuration of its own.
set -e
HERE=$(cd "$(dirname "$0")" && pwd)
# shellcheck disable=SC2086
lazbuild $LAZBUILD_OPTIONS "$HERE/../dclBold.lpk"
# shellcheck disable=SC2086
lazbuild $LAZBUILD_OPTIONS "$HERE/BoldRegistrationCheck.lpi"

"$HERE/bin/BoldRegistrationCheck"
