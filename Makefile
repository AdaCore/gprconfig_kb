all:
	gnatmake -Pgprconfig

clean:
	gnat clean -q -Pgprconfig
	${RM} standard_foo.gpr

distclean: clean
	${RM} config.log config.status doc/sdefault.ads

gprconfig.pdf: gprconfig.texi
	cd doc; echo x | texi2dvi -p $<
	cd doc; ${RM} gprconfig.cp gprconfig.log gprconfig.ky gprconfig.toc
	cd doc; ${RM} gprconfig.aux gprconfig.fn gprconfig.pg
	cd doc; ${RM} gprconfig.tp gprconfig.cps gprconfig.vr

test: all force
	${RM} standard_foo.gpr
	./gprconfig -o standard_foo.gpr -config GNAT,/usr/local/gnat-5.05w/bin -batch
	@cat standard_foo.gpr

force:

