all:
	gnatmake -Pgprconfig

clean:
	gnat clean -q -Pgprconfig
	${RM} standard.gpr

distclean: clean
	${RM} config.log config.status sdefault.ads

gprconfig.pdf: gprconfig.texi
	echo x | texi2dvi -p $<
	

test: all force
	./gprconfig 
	cat standard.gpr

force:

