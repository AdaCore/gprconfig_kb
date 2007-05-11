all: default.cgpr
	gprbuild animals.gpr
	gprbuild except.gpr

default.cgpr:
	gprconfig -batch -config GNAT,,,Ada,,sjlj  -config G++ -o $@

clean: force
	gprclean -r -Panimals
	gprclean -r -Pexcept
	$(RM) default.cgpr
	$(RM) ada_runtime.gpr

run: all
	ada_main
	cpp_main
	main
force:
