all: default.cgpr
	gprbuild -p -Pc_main
	gprbuild -p -Pada_main

default.cgpr:
	gprconfig --batch --config Ada  --config C -o $@

clean: force
	gprclean -Pc_main
	gprclean -Pada_main
	$(RM) default.cgpr

run: all
	./ada_main
	./obj1/c_main
force:
