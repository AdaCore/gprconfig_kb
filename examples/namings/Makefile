all: default.cgpr
	gprbuild -p -Pnamings -XC1=case1
	gprbuild -p -Pnamings -XC1=case2
	gprbuild -p -Pnamings -XC1=case3

default.cgpr:
	gprconfig --batch --config Ada  --config C -o $@

clean: force
	gprclean -Pnamings -XC1=case1
	gprclean -Pnamings -XC1=case2
	gprclean -Pnamings -XC1=case3
	$(RM) default.cgpr

run: all
	./main-case1
	./main-case2
	./main-case3
force:
