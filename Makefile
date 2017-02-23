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
#   TARGET        : target triplet for cross-compilation
#
# Out-of-tree build:
#
#   $ make -f <GPRBUILD_TREE>/Makefile setup
#   $ make -f <GPRBUILD_TREE>/Makefile

HOST    = $(shell gcc -dumpmachine)
TARGET := $(shell gcc -dumpmachine)

prefix	      := $(dir $(shell which gnatls))..
BUILD         = production
PROCESSORS    = 0
BUILD_DIR     =
SOURCE_DIR    := $(shell dirname "$(MAKEFILE_LIST)")

# Load current setup if any
-include makefile.setup

# target options for cross-build
ifeq ($(HOST),$(TARGET))
GTARGET=
else
GTARGET=--target=$(TARGET)
endif

# check for out-of-tree build
ifeq ($(SOURCE_DIR),.)
RBD=
GPRBUILD_GPR=gprbuild.gpr
GPR_GPR=gpr/gpr.gpr
MAKEPREFIX=
else
RBD=--relocate-build-tree
GPRBUILD_GPR=$(SOURCE_DIR)/gprbuild.gpr
GPR_GPR=$(SOURCE_DIR)/gpr/gpr.gpr
MAKEPREFIX=$(SOURCE_DIR)/
endif

ENABLE_SHARED := $(shell gprbuild $(GTARGET) -c -q -p \
	-P$(MAKEPREFIX)config/test_shared 2>/dev/null && echo "yes")

ifeq ($(ENABLE_SHARED), yes)
   LIBGPR_TYPES=static shared static-pic
else
   LIBGPR_TYPES=static
endif

# Used to pass extra options to GPRBUILD, like -d for instance
GPRBUILD_OPTIONS=

BUILDER=gprbuild -p -m $(GTARGET) $(RBD) -j${PROCESSORS} -XBUILD=${BUILD} ${GPRBUILD_OPTIONS}
LIB_INSTALLER=gprinstall -p -f --target=$(TARGET) $(RBD) --prefix=${prefix}
INSTALLER=exe/$(LIB_INSTALLER)
CLEANER=gprclean -q $(RBD)

GPRBUILD_BUILDER=$(BUILDER) $(GPRBUILD_GPR) \
	-XLIBRARY_TYPE=static -XXMLADA_BUILD=static
LIBGPR_BUILDER=$(BUILDER) $(GPR_GPR)
LIBGPR_INSTALLER=$(LIB_INSTALLER) $(GPR_GPR) -XBUILD=${BUILD} \
	--install-name=gpr --build-var=LIBRARY_TYPE $(GTARGET)
LIBGPR_UNINSTALLER=$(LIB_INSTALLER) $(GPR_GPR) --install-name=gpr --uninstall

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
	$(INSTALLER) --mode=usage --install-name=gprbuild \
		-XINSTALL_MODE=nointernal $(GPRBUILD_GPR)
	$(INSTALLER) --target=$(TARGET) --mode=usage  --install-name=gprbuild \
		-XINSTALL_MODE=internal $(GPRBUILD_GPR)

complete: all install libgpr.install.static

##########
# Libgpr #
##########

.PHONY: libgpr.build libgpr.build.static libgpr.build.shared libgpr.build.static-pic
.PHONY: libgpr.install libgpr.install.static libgpr.install.shared libgpr.install.static-pic
.PHONY: libgpr.uninstall

libgpr.build: $(foreach t, $(LIBGPR_TYPES), libgpr.build.$(t))

libgpr.build.shared:
	${BUILDER} -XLIBRARY_TYPE=relocatable \
		-XXMLADA_BUILD=relocatable $(GPR_GPR)

libgpr.build.static:
	${BUILDER} -XLIBRARY_TYPE=static \
		-XXMLADA_BUILD=static $(GPR_GPR)

libgpr.build.static-pic:
	${BUILDER} -XLIBRARY_TYPE=static-pic \
		-XXMLADA_BUILD=static-pic $(GPR_GPR)

libgpr.install: libgpr.uninstall $(foreach t, $(LIBGPR_TYPES), libgpr.install.$(t))

libgpr.install.static:
	$(LIBGPR_INSTALLER) \
	   -XLIBRARY_TYPE=static \
	   -XXMLADA_BUILD=static \
	   --lib-subdir=lib/gpr/static \
	   --build-name=static

libgpr.install.static-pic:
	$(LIBGPR_INSTALLER) \
	   -XLIBRARY_TYPE=static-pic \
	   -XXMLADA_BUILD=static-pic \
	   --lib-subdir=lib/gpr/static-pic \
	   --build-name=static-pic

libgpr.install.shared:
	$(LIBGPR_INSTALLER) \
	   -XLIBRARY_TYPE=relocatable \
	   -XXMLADA_BUILD=relocatable \
	   --lib-subdir=lib/gpr/relocatable \
	   --build-name=relocatable

libgpr.uninstall:
	-$(LIBGPR_UNINSTALLER)

libgpr.clean:
	-$(CLEANER) -XLIBRARY_TYPE=relocatable $(GPR_GPR)
	-$(CLEANER) -XLIBRARY_TYPE=static $(GPR_GPR)
	-$(CLEANER) -XLIBRARY_TYPE=static-pic $(GPR_GPR)
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
	echo "SOURCE_DIR=$(SOURCE_DIR)" >> makefile.setup

###################
# Cleanup targets #
###################

.PHONY: clean
clean: libgpr.clean
	-$(CLEANER) -r -Pgprbuild -XBUILD=$(BUILD)
	-$(CLEANER) -r -XBUILD=${BUILD} \
		-XLIBRARY_TYPE=static-pic $(GPR_GPR)
	-$(CLEANER) -r -XBUILD=${BUILD} \
		-XLIBRARY_TYPE=static $(GPR_GPR)
ifeq ($(ENABLE_SHARED), yes)
	-$(CLEANER) -r -XBUILD=${BUILD} \
		-XLIBRARY_TYPE=relocatable $(GPR_GPR)
	-$(CLEANER) $(GPR_GPR) -XBUILD=$(BUILD) \
		-XLIBRARY_TYPE=relocatable
endif
	make -C $(MAKEPREFIX)doc clean
	make -C $(MAKEPREFIX)examples clean
	rm -fr obj exe makefile.setup

.PHONY: doc examples

doc:
	make -C $(MAKEPREFIX)doc

examples: force
	make -C $(MAKEPREFIX)examples

force:
