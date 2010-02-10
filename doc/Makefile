## The documentation packages are built in subdirectories, to match
## what is done in AdaCore's automatic packaging

DOC=gprbuild_ug
MAKEINFO_HTML_FLAGS=--html --number-sections --css-include=gprbuild.css --no-headers --no-split

MAKE_INFO=makeinfo --force --no-split
MAKE_TXT=$(MAKE_INFO) --no-headers --fill-column=79
MAKE_PDF=texi2pdf -q
MKDIR=mkdir -p
RMDIR=rmdir
CP=cp
EXE=

.PHONY: all txt info html pdf clean

all: txt info html pdf

# The following are written to make sure the build fails if the output file
# already exists, and cannot be overwritten. For example, if we have
# pdf/gprbuild_ug.pdf open in a browser on windows, the $(MAKE_PDF) command
# will complain:
#    cp: cannot create regular file `.../gprbuild_ug.pdf': Device or resource busy
# but still returns a zero (successful) exit status. Therefore, we delete the
# output first, so the ${RMDIR} will correctly return a non-zero status.
# But if pdf does not exist, then the ${RMDIR} command will incorrectly fail;
# therefore, we do ${MKDIR} first -- hence we do ${MKDIR} twice, which is a bit
# odd, but successfully works around the bug in $(MAKE_PDF) (i.e. texi2pdf).

txt: prepare
	${MKDIR} txt
	${RM} txt/*
	${RMDIR} txt
	${MKDIR} txt
	$(MAKE_TXT) -o txt/$(DOC).txt $(DOC).texi
info: prepare
	${MKDIR} info
	${RM} info/*
	${RMDIR} info
	${MKDIR} info
	$(MAKE_INFO) -o info/$(DOC).info $(DOC).texi
html: prepare
	${MKDIR} html
	${RM} html/*
	${RMDIR} html
	${MKDIR} html
	$(MAKE_INFO) ${MAKEINFO_HTML_FLAGS} -o html/$(DOC).html $(DOC).texi
	$(CP) important.png note.png tip.png html/
pdf: prepare
	${MKDIR} pdf
	${RM} pdf/*
	${RMDIR} pdf
	${MKDIR} pdf
	cd pdf; $(MAKE_PDF) -c ../$(DOC).texi

prepare:
	gnatmake ../gnat/xgnatugn
	xgnatugn${EXE} unw ../gnat/projects.texi ../gnat/ug_words projects_unw.texi

partial_clean:
	${RM} ${DOC}.aux ${DOC}.cp ${DOC}.cps ${DOC}.cps ${DOC}.fn
	${RM} ${DOC}.ky ${DOC}.log ${DOC}.pg ${DOC}.toc ${DOC}.tp
	${RM} ${DOC}.vr
	${RM} xgnatugn* projects_unw.texi

clean: partial_clean
	${MKDIR} txt html info pdf
	${RM} txt/* html/* info/* pdf/*
	${RMDIR} txt html info pdf
