DOC=gprbuild_ug
MAKE_INFO=makeinfo --force --no-split
MAKE_TXT=$(MAKE_INFO) --no-headers --fill-column=79
MAKE_HTML=$(MAKE_INFO) --html --number-sections --css-include=../gnat_src/gnat_style.css 
MAKE_HTML2=${MAKE_HTML} --no-headers --css-include=gprbuild.css
MAKE_PDF=texi2pdf -q

.PHONY: all txt info html pdf clean

all: txt info html html2 pdf

txt:
	$(MAKE_TXT) -o $(DOC).txt $(DOC).texi
info:
	$(MAKE_INFO) -o $(DOC).info $(DOC).texi
html:
	$(MAKE_HTML) -o $(DOC).html $(DOC).texi
html2:
	$(MAKE_HTML2) -o $(DOC)_2.html $(DOC).texi
pdf:
	$(MAKE_PDF)  -o $(DOC).pdf $(DOC).texi

clean:
	${RM} $(DOC).txt $(DOC).html $(DOC).info $(DOC).pdf
