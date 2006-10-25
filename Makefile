all:
	gnatmake -Pgprconfig

clean:
	gnat clean -q -Pgprconfig
	${RM} standard.gpr

distclean: clean
	${RM} config.log config.status sdefault.ads

test: all force
	./gprconfig 
	cat standard.gpr

force:

