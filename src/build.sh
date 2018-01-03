#!/usr/bin/env bash

set -eu

cd "$(dirname $0)"

USE_MONO=false
SHOW_HELP=false
while test $# -gt 0; do
  case "$1" in
    -m|--use-mono)
      USE_MONO=true
      shift
      ;;
    --)
      shift
      break
      ;;
    -h|--help)
      SHOW_HELP=true
      break
      ;;
    *)
      echo "unknown option $1"
      SHOW_HELP=true
      break
      ;;
  esac
done

if [ "$SHOW_HELP" = true ]; then
  echo "usage: ./build.sh [-m|--use-mono] [--] [<fake options>]"
  echo ""
  echo "  -m|--use-mono   Invoke paket and fake using mono (requires that mono is installed)"
  echo "  --              All options after this are passed on to Fake."
  echo "  -h|--help       Show help and exit"
  echo ""
  echo "For example, to run the Test target under mono: ./build.sh --use-mono -- Test"
  exit 1
fi

run() {
  if [ "$USE_MONO" == true ]; then
    mono "$@"
  else
    "$@"
  fi
}

PAKET_EXE='.paket/paket.exe'
run $PAKET_EXE restore

FAKE_EXE="`find packages/build -iname fake.exe`"
run $FAKE_EXE "$@" $FSIARGS $FSIARGS2 build.fsx
