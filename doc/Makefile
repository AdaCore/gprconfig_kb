## The documentation packages are built in subdirectories, to match
## what is done in AdaCore's automatic packaging

DOC=gprbuild_ug
MAKEINFO_HTML_FLAGS=--html --number-sections --css-include=gprbuild.css --no-headers --no-split

MAKE_INFO=makeinfo --force --no-split
MAKE_TXT=$(MAKE_INFO) --no-headers --fill-column=79
MAKE_PDF=texi2pdf -q
MKDIR=mkdir -p
RMDIR=rmdir

.PHONY: all txt info html pdf clean

all: txt info html pdf

txt:
	${MKDIR} txt
	$(MAKE_TXT) -o txt/$(DOC).txt $(DOC).texi
info:
	${MKDIR} info
	$(MAKE_INFO) -o info/$(DOC).info $(DOC).texi
html:
	${MKDIR} html
	$(MAKE_INFO) ${MAKEINFO_HTML_FLAGS} -o html/$(DOC).html $(DOC).texi
pdf:
	${MKDIR} pdf
	cd pdf; $(MAKE_PDF) -c ../$(DOC).texi

partial_clean:
	${RM} ${DOC}.aux ${DOC}.cp ${DOC}.cps ${DOC}.cps ${DOC}.fn
	${RM} ${DOC}.ky ${DOC}.log ${DOC}.pg ${DOC}.toc ${DOC}.tp
	${RM} ${DOC}.vr

clean: partial_clean
	${RM} txt/* html/* info/* pdf/*
	-${RMDIR} txt html info pdf
