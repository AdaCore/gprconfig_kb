####################################################################
## Checking for GNAT sources
####################################################################

AC_DEFUN(AM_GNAT_SOURCES,
[
   AC_MSG_CHECKING(GNAT sources)
   AC_ARG_WITH(gnat-source-dir,
               [  --with-gnat-source-dir=<dir>    Directory that contains the GNAT sources],
               GNAT_SOURCE_DIR=$withval,
               GNAT_SOURCE_DIR="gnat")
   if ( test -f $GNAT_SOURCE_DIR/prj.ads ) then
       AC_MSG_RESULT(yes)
   else
       AC_MSG_RESULT(no)
       AC_MSG_ERROR("GNAT sources not found in '$GNAT_SOURCE_DIR'. Check the --with-gnat-source-dir switch")
   fi
]
)
