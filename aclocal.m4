
AC_DEFUN(AM_CHECK_XMLADA,
[
  AC_MSG_CHECKING(for xmlada)
  xmlada_build_target=
  xmlada_prj_flags=

  if test -d $srcdir/xmlada; then
     xmlada_build_target=build_xmlada
     xmlada_prj_flags="-aP$srcdir/xmlada/install/lib/gnat"
     AC_MSG_RESULT(yes (sources in gprbuild tree))
  else
     # Create a temporary directory (from "info autoconf")
     : ${TMPDIR=/tmp}
     {
       tmp=`(umask 077 && mktemp -d "$TMPDIR/fooXXXXXX") 2>/dev/null` \
        && test -n "$tmp" && test -d "$tmp"
     } || {
       tmp=$TMPDIR/foo$$-$RANDOM
       (umask 077 && mkdir -p "$tmp")
     } || exit $?

     mkdir $tmp/lib
     cat > $tmp/conftest.gpr << EOF
with "xmlada.gpr";

project Conftest is
  for Source_Dirs use ();
end Conftest;
EOF
     if gnatmake -P$tmp/conftest.gpr >&AS_MESSAGE_LOG_FD 2>&1; then
        AC_MSG_RESULT(yes (precompiled))
     else
        AC_MSG_RESULT(no)
        AC_MSG_FAILURE(cannot find xmlada)
     fi
  fi
  AC_SUBST(xmlada_build_target)
  AC_SUBST(xmlada_prj_flags)
])
