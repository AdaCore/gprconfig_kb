
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
  for Main use ("main.adb");
end Conftest;
EOF
     cat > $tmp/main.adb << EOF
procedure Main is
begin null; end;
EOF
     if gprbuild -c -P$tmp/conftest.gpr >&5 2>&1; then
        AC_MSG_RESULT(yes (precompiled))
     else
        AC_MSG_RESULT(no)
        AC_MSG_FAILURE(cannot find xmlada)
     fi
  fi
  AC_SUBST(xmlada_build_target)
  AC_SUBST(xmlada_prj_flags)
])

AC_DEFUN(AM_GNAT_BUILDS_SHARED,
[
   AC_MSG_CHECKING(whether gnat can build shared libs)
   gnat_builds_shared=

   mkdir conftest conftest/lib
   echo "package Foo is end Foo;" > conftest/foo.ads
   cat > conftest/lib.gpr <<EOF
project Lib is
   for Source_Dirs use (".");
   for Library_Dir use "lib";
   for Library_Name use "lib";
   for Library_Kind use "relocatable";
end Lib;
EOF

   if AC_TRY_COMMAND([gprbuild -c -q -Pconftest/lib]); then
      gnat_builds_shared=yes
   else
      gnat_builds_shared=no
   fi
      rm -rf conftest
      AC_MSG_RESULT($gnat_builds_shared)

   AC_SUBST(gnat_builds_shared)
]) 
