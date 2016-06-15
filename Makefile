# Makefile targets
# ----------------
#
# Setup:                   make [VAR=VALUE] setup (see below)
# Build gprbuild:          make all
# Install gprbuild:        make install
# Create gprbuild package: make distall
# Build tool <TOOL>:       make <TOOL>
# Build libgpr:            make libgpr.build
# Install libgpr:          make libgpr.install
# (for libgpr you can add ".type" where type is static, static-pic or shared
#  build a specific version of the lib. by default all supported variants are
#  built).

# Variables which can be set:
#
#   ENABLE_SHARED : yes / no (or empty)
#   BUILD         : debug production coverage profiling
#   PROCESSORS    : nb parallel compilations (0 to use all cores)
#   TARGET        : traget triplet for cross-compilation

ENABLE_SHARED = $(shell gnat make -c -q -p \
			-Pconfig/test_shared 2>/dev/null && echo "yes")

TARGET 	      = $(shell gcc -dumpmachine)
prefix	      = $(dir $(shell which gnatls))..
BUILD         = production
PROCESSORS    = 0

HOST = $(shell gcc -dumpmachine)

ifeq ($(ENABLE_SHARED), yes)
   LIBGPR_TYPES=static shared static-pic
else
   LIBGPR_TYPES=static
endif

# Load current setup if any
-include makefile.setup

# target options for cross-build
ifeq ($(HOST),$(TARGET))
GTARGET=
else
GTARGET=--target=$(TARGET)
endif

BUILDER=gprbuild -p -m $(GTARGET) -j${PROCESSORS} -XBUILD=${BUILD}
INSTALLER=gprinstall -p -f --prefix=${prefix}

GPRBUILD_BUILDER=$(BUILDER) -Pgprbuild -XLIBRARY_TYPE=static
LIBGPR_BUILDER=$(BUILDER) -Pgpr/gpr.gpr
LIBGPR_INSTALLER=$(INSTALLER) gpr/gpr.gpr \
	--install-name=gpr --build-var=LIBRARY_TYPE $(GTARGET)
LIBGPR_UNINSTALLER=$(INSTALLER) gpr/gpr.gpr -p -f \
   --install-name=gpr --uninstall

#########
# build #
#########

.PHONY: all distall gprbuild gprconfig gprclean gprinstall gprname gprls

build all:
	$(GPRBUILD_BUILDER)

distall: all install

gprbuild:
	$(GPRBUILD_BUILDER) gprbuild-main.adb

gprinstall:
	$(GPRBUILD_BUILDER) gprinstall-main.adb

gprclean:
	$(GPRBUILD_BUILDER) gprclean-main.adb

gprconfig:
	$(GPRBUILD_BUILDER) gprconfig-main.adb

gprname:
	$(GPRBUILD_BUILDER) gprname-main.adb

gprls:
	$(GPRBUILD_BUILDER) gprls-main.adb

#################################
# Gprbuild installation targets #
#################################

.PHONY: install

install:
	$(INSTALLER) --mode=usage gprbuild.gpr

complete: all install libgpr.install.static

##########
# Libgpr #
##########

.PHONY: libgpr.build libgpr.build.static libgpr.build.shared libgpr.build.static-pic
.PHONY: libgpr.install libgpr.install.static libgpr.install.shared libgpr.install.static-pic
.PHONY: libgpr.uninstall

libgpr.build: $(foreach t, $(LIBGPR_TYPES), libgpr.build.$(t))

libgpr.build.shared:
	${BUILDER} -XLIBRARY_TYPE=relocatable -Pgpr/gpr.gpr

libgpr.build.static:
	${BUILDER} -XLIBRARY_TYPE=static -Pgpr/gpr.gpr

libgpr.build.static-pic:
	${BUILDER} -XLIBRARY_TYPE=static-pic -Pgpr/gpr.gpr

libgpr.install: $(foreach t, $(LIBGPR_TYPES), libgpr.install.$(t))

libgpr.install.static:
	$(LIBGPR_INSTALLER) \
	   -XLIBRARY_TYPE=static \
	   --lib-subdir=lib/gpr/static \
	   --build-name=static

libgpr.install.static-pic:
	$(LIBGPR_INSTALLER) \
	   -XLIBRARY_TYPE=static-pic \
	   --lib-subdir=lib/gpr/static-pic \
	   --build-name=static-pic

libgpr.install.shared:
	$(LIBGPR_INSTALLER) \
	   -XLIBRARY_TYPE=relocatable \
	   --lib-subdir=lib/gpr/relocatable \
	   --build-name=relocatable

libgpr.uninstall:
	-$(LIBGPR_UNINSTALLER)

libgpr.clean:
	-gprclean -XLIBRARY_TYPE=relocatable -Pgpr/gpr.gpr
	-gprclean -XLIBRARY_TYPE=static -Pgpr/gpr.gpr
	-gprclean -XLIBRARY_TYPE=static-pic -Pgpr/gpr.gpr
	rm -fr gpr/lib gpr/libobj

#########
# setup #
#########

.SILENT: setup
setup:
	echo "prefix=$(prefix)" > makefile.setup
	echo "ENABLE_SHARED=$(ENABLE_SHARED)" >> makefile.setup
	echo "BUILD=$(BUILD)" >> makefile.setup
	echo "PROCESSORS=$(PROCESSORS)" >> makefile.setup
	echo "TARGET=$(TARGET)" >> makefile.setup

###################
# Cleanup targets #
###################

.PHONY: clean
clean: libgpr.clean
	-gprclean -q -r -Pgprbuild -XBUILD=$(BUILD)
	-gprclean -q -r -XBUILD=${BUILD} \
		-XLIBRARY_TYPE=static-pic -Pgpr/gpr.gpr
	-gprclean -q -r -XBUILD=${BUILD} \
		-XLIBRARY_TYPE=static -Pgpr/gpr.gpr
ifeq ($(ENABLE_SHARED), yes)
	-gprclean -q -r -XBUILD=${BUILD} \
		-XLIBRARY_TYPE=relocatable -Pgpr/gpr.gpr
	-gprclean -q -Pgpr/gpr.gpr -XBUILD=$(BUILD) \
		-XLIBRARY_TYPE=relocatable
endif
	rm -fr obj exe makefile.setup
	make -C doc clean
	make -C examples clean

.PHONY: doc examples

doc:
	make -C doc

examples: force
	make -C examples

force:
