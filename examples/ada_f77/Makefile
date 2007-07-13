all: default.cgpr
	gprbuild ft1.gpr
	gprbuild ft2.gpr
	gprbuild ft3.gpr

default.cgpr:
	gprconfig -batch -config GNAT  -config G77 -o $@

clean:
	gprclean -q -r ft1.gpr
	gprclean -q -r ft2.gpr
	gprclean -q -r ft3.gpr
	$(RM) default.cgpr

run: all
	fobj1/ess
	fobj2/ess2
	fobj2/ess3
