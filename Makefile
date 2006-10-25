all:
	gnatmake -Pgprconfig

clean:
	gnat clean -q -Pgprconfig
	${RM} standard.gpr

distclean: clean
	${RM} config.log config.status sdefault.ads

gprconfig.pdf: gprconfig.texi
	echo x | texi2dvi -p $<
	${RM} gprconfig.cp gprconfig.log gprconfig.ky gprconfig.toc
	${RM} gprconfig.aux gprconfig.fn gprconfig.pg
	${RM} gprconfig.tp gprconfig.cps gprconfig.vr

test: all force
	./gprconfig 
	cat standard.gpr

force:

