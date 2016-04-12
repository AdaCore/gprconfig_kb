all: default.cgpr
	gprbuild -p ft.gpr

default.cgpr:
	gprconfig --batch --config Ada --config Fortran -o $@

clean:
	gprclean -q -r ft.gpr
	$(RM) default.cgpr

run: all
	./fobj/ess
