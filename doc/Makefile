DOC=gprbuild_ug
pdf:
	../../gnat/auxtools/ada2texi$(EXE) $(DOC).texi $(DOC)_hld.texi
	texi2pdf $(DOC)_hld.texi -o $(DOC).pdf
info:
	makeinfo $(DOC).texi