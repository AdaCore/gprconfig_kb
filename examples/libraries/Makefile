all: default.cgpr
	gprbuild -p main0.gpr
	gprbuild -p main1.gpr
	gprbuild -p main2.gpr
	gprbuild -p main3.gpr
	gprbuild -p main4.gpr

default.cgpr:
	gprconfig --batch --config Ada  --config C -o $@

clean: force
	gprclean -r main0.gpr
	gprclean -r main1.gpr
	gprclean -r main2.gpr
	gprclean -r main3.gpr
	gprclean -r main4.gpr
	$(RM) default.cgpr

run: all
	./main0
	./main1
	(PATH="lib2:${PATH}" ./main2)
	(PATH="lib3:${PATH}" ./main3)
	(PATH="lib2:${PATH}" ./main4)

force:
