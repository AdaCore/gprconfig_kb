all: default.cgpr
	gprbuild -Pc_main
	gprbuild -Pada_main

default.cgpr:
	gprconfig -batch -config GNAT,,,Ada,,default  -config GCC,,,C -o $@

clean: force
	gprclean -r -Pc_main
	gprclean -r -Pada_main
	$(RM) default.cgpr

run: all
	ada_main
	c_main
force:
