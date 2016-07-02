all: default.cgpr
	gprbuild -p -Pc_main
	gprbuild -p -Pada_main

default.cgpr: force
	gprconfig --batch --config Ada --config C -o $@

clean: force
	gprclean -r -Pc_main
	gprclean -r -Pada_main
	$(RM) default.cgpr

run: all
	./ada_main
	./c_main
force:
