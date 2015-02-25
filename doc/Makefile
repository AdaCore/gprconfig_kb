# Makefile for Sphinx documentation

# You can set these variables from the command line.
SPHINXOPTS    =
SPHINXBUILD   = sphinx-build
PAPER         =
BUILDDIR      = build
SOURCEDIR     = .

# Internal variables.
PAPEROPT_a4     = -D latex_paper_size=a4
PAPEROPT_letter = -D latex_paper_size=letter
ALLSPHINXOPTS   = $(PAPEROPT_$(PAPER)) $(SPHINXOPTS) \
		  -c $(SOURCEDIR)/share \
		  -d $(BUILDDIR)/$*/doctrees \
		  $(SOURCEDIR)
FMT_LIST=html txt info pdf
DOC_NAME=gprbuild_ug
CP=cp

.PHONY: help clean

all: $(foreach fmt, $(FMT_LIST), $(fmt))

help:
	@echo "Please use \`make <target>' where <target> is one of"
	@echo "  html       to make standalone HTML files"
	@echo "  pdf        to make LaTeX files and run them through pdflatex"
	@echo "  txt        to make text files"
	@echo "  texinfo    to make Texinfo files"
	@echo "  info       to make info files"
	@echo "  all                 to build documentation in all formats"
	@echo ""
	@echo "source and location can be overriden using SOURCEDIR and BUILDDIR variables"

prepare:
	$(CP) ../gnat/gnat_project_manager.rst share/.
	$(CP) ../gnat/project-manager-figure.png share/.
	$(CP) ../gnat/gnu_free_documentation_license.rst share/.
	$(CP) ../gnat/ada_pygments.py share/.
	$(CP) ../gnat/favicon.ico share/.
	$(CP) ../gnat/latex_elements.py share/.
	$(CP) ../gnat/sphinx.sty share/.

clean:
	-rm -rf $(BUILDDIR)/html \
		$(BUILDDIR)/pdf \
		$(BUILDDIR)/txt \
		$(BUILDDIR)/info

html: prepare
	$(SPHINXBUILD) -b html $(ALLSPHINXOPTS) $(BUILDDIR)/html

pdf: prepare
	$(SPHINXBUILD) -b latex $(ALLSPHINXOPTS) $(BUILDDIR)/pdf
	cp $(SOURCEDIR)/share/sphinx.sty $(BUILDDIR)/pdf
	$(MAKE) -C $(BUILDDIR)/pdf all-pdf LATEXOPTS="-interaction=nonstopmode"

txt: prepare
	$(SPHINXBUILD) -b texinfo $(ALLSPHINXOPTS) $(BUILDDIR)/$*/txt
	$(MAKE) -C $(BUILDDIR)/txt plaintext

info: prepare
	$(SPHINXBUILD) -b texinfo $(ALLSPHINXOPTS) $(BUILDDIR)/$*/info
	$(MAKE) -C $(BUILDDIR)/info info

texinfo:
	$(SPHINXBUILD) -b texinfo $(ALLSPHINXOPTS) $(BUILDDIR)/$*/texinfo
