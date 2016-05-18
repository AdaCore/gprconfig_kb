all: default.cgpr
	gprbuild -p animals.gpr
	gprbuild -p except.gpr

default.cgpr:
	gprconfig --batch --config Ada  --config C++ -o $@

clean: force
	gprclean -r -Panimals
	gprclean -r -Pexcept
	$(RM) default.cgpr

run: all
	./ada_main
	./cpp_main
	./main
force:
