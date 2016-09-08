#!/bin/sh

# --prefix=
# --with-xmlada=

xmlada_src=../xmlada.git
prefix=/usr/local
CC=gcc
GNATMAKE=gnatmake
CFLAGS=-O
GNATMAKEFLAGS=-j0
INSTALL="cp"

progname=$0
show_help=no

# Find srcdir
srcdir=`dirname $progname`
if test x$srcdir = x; then
    srcdir=.
fi

# Decode options
for opt do
  optarg=`expr x"$opt" : 'x[^=]*=\(.*\)'`
  case "$opt" in
      CC=*|CFLAGS=*|GNATMAKE=*|GNATMAKEFLAGS)
        optvar=`expr x"$opt" : 'x\([^=]*\)=.*'`
	eval $optvar=\"$optarg\"
	;;
      --prefix=*) prefix="$optarg";;
      --srcdir=*) srcdir="$optarg";;
      --with-xmlada=*) xmlada_src="$optarg";;
      -h|-help|--help) show_help=yes;;
      *) echo "$0: unknown option $opt; try $0 --help"
	  exit 1
	  ;;
  esac
done

# Help
if test $show_help != no; then
cat <<EOF
Usage: bootstrap.sh [options]

Options [defaults in brackets]:
  --prefix=PREFIX             install in PREFIX [$prefix]
  --srcdir=SRCDIR             source code path [$srcdir]
  --with-xmlada=DIR           XML-Ada source path [$xmlada_src]
  --CC=cc                     specify C compiler [$CC]
  --GNATMAKE=gnatmake         specify gnatmake Ada builder [$GNATMAKE]
  --CFLAGS=                   set C and Ada compilation flags [$CFLAGS]
  --GNATMAKEFLAGS=            Ada builder flags [$GNATMAKEFLAGS]
EOF
exit 0
fi

# Exit in case of error
set -e

INC_FLAGS="-I${srcdir}/src -I${srcdir}/gpr/src -I${xmlada_src}/sax -I${xmlada_src}/dom -I${xmlada_src}/schema -I${xmlada_src}/unicode -I${xmlada_src}/input_sources"

# Programs to build and install
BIN_PROGS="gprbuild gprconfig gprclean gprinstall gprname gprls"
LIBEXEC_PROGS="gprlib gprbind"

# Build
${CC} -c $CFLAGS $srcdir/src/gpr_imports.c
for prg in $BIN_PROGS; do
    ${GNATMAKE} $INC_FLAGS ${prg}-main -o $prg $CFLAGS $GNATMAKEFLAGS -largs gpr_imports.o
done

for prg in $LIBEXEC_PROGS; do
    ${GNATMAKE} $INC_FLAGS $prg $CFLAGS $GNATMAKEFLAGS -largs gpr_imports.o
done

# Install
mkdir -p $prefix/bin
$INSTALL $BIN_PROGS $prefix/bin
mkdir -p $prefix/libexec/gprbuild
$INSTALL $LIBEXEC_PROGS $prefix/libexec/gprbuild
mkdir -p $prefix/share/gprconfig
$INSTALL $srcdir/share/gprconfig/*.xml $prefix/share/gprconfig
mkdir -p $prefix/share/gpr
$INSTALL $srcdir/share/_default.gpr $prefix/share/gpr

# Done
exit 0
