all: default.cgpr
	gprbuild

default.cgpr:
	gprconfig --batch --config Ada  --config C --config Fortran -o $@

clean: force
	gprclean
	$(RM) default.cgpr

run: all
	obj/main
force:
