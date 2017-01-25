/****************************************************************************
 *                                                                          *
 *                             GPR TECHNOLOGY                               *
 *                                                                          *
 *                      Copyright (C) 1992-2017, AdaCore                    *
 *                                                                          *
 * This is  free  software;  you can redistribute it and/or modify it under *
 * terms of the  GNU  General Public License as published by the Free Soft- *
 * ware  Foundation;  either version 3,  or (at your option) any later ver- *
 * sion.  This software is distributed in the hope  that it will be useful, *
 * but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN- *
 * TABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public *
 * License for more details.  You should have received  a copy of the  GNU  *
 * General Public License distributed with GNAT; see file  COPYING. If not, *
 * see <http://www.gnu.org/licenses/>.                                      *
 *                                                                          *
 ****************************************************************************/

#ifdef __cplusplus
extern "C" {
#endif

#ifdef IN_GCC
#include "auto-host.h"
#endif

#include <string.h>

/*  link_max is a conservative system specific threshold (in bytes) of the  */
/*  argument length passed to the linker which will trigger a file being    */
/*  used instead of the command line directly.                              */

/*  shared_libgcc_default gives the system dependent link method that       */
/*  be used by default for linking libgcc (shared or static)                */

/*  default_libgcc_subdir is the subdirectory name (from the installation   */
/*  root) where we may find a shared libgcc to use by default.              */

#define SHARED 'H'
#define STATIC 'T'

#if defined (__WIN32)
int __gnat_link_max = 30000;
char __gnat_shared_libgcc_default = STATIC;
const char *__gnat_default_libgcc_subdir = "lib";

#elif defined (__hpux__)
int __gnat_link_max = 5000;
char __gnat_shared_libgcc_default = STATIC;
const char *__gnat_default_libgcc_subdir = "lib";

#elif defined (__FreeBSD__)
int __gnat_link_max = 8192;
char __gnat_shared_libgcc_default = STATIC;
const char *__gnat_default_libgcc_subdir = "lib";

#elif defined (__APPLE__)
int __gnat_link_max = 262144;
char __gnat_shared_libgcc_default = SHARED;
const char *__gnat_default_libgcc_subdir = "lib";

#elif defined (linux) || defined(__GLIBC__)
int __gnat_link_max = 8192;
char __gnat_shared_libgcc_default = STATIC;
#if defined (__x86_64)
# if defined (__LP64__)
const char *__gnat_default_libgcc_subdir = "lib64";
# else
const char *__gnat_default_libgcc_subdir = "libx32";
# endif
#else
const char *__gnat_default_libgcc_subdir = "lib";
#endif

#elif defined (_AIX)
int __gnat_link_max = 15000;
char __gnat_shared_libgcc_default = STATIC;
const char *__gnat_default_libgcc_subdir = "lib";

#elif (HAVE_GNU_LD)
int __gnat_link_max = 8192;
char __gnat_shared_libgcc_default = STATIC;
const char *__gnat_default_libgcc_subdir = "lib";

#elif defined (sun)
int __gnat_link_max = 2147483647;
char __gnat_shared_libgcc_default = STATIC;
#if defined (__sparc_v9__) || defined (__sparcv9)
const char *__gnat_default_libgcc_subdir = "lib/sparcv9";
#elif defined (__x86_64)
const char *__gnat_default_libgcc_subdir = "lib/amd64";
#else
const char *__gnat_default_libgcc_subdir = "lib";
#endif

#elif defined (__svr4__) && defined (i386)
int __gnat_link_max = 2147483647;
char __gnat_shared_libgcc_default = STATIC;
const char *__gnat_default_libgcc_subdir = "lib";

#else
int __gnat_link_max = 2147483647;
char __gnat_shared_libgcc_default = STATIC;
const char *__gnat_default_libgcc_subdir = "lib";
#endif

#ifdef __cplusplus
}
#endif
