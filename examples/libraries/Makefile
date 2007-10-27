all: default.cgpr
	gprbuild main1.gpr
	gprbuild main2.gpr
	gprbuild main3.gpr
	gprbuild main4.gpr
	gprbuild main5.gpr

default.cgpr:
	gprconfig --batch --config Ada,,default  --config C -o $@

clean: force
	gprclean -r main1.gpr
	gprclean -r main2.gpr
	gprclean -r main3.gpr
	gprclean -r main4.gpr
	gprclean -r main5.gpr
	$(RM) default.cgpr

run: all
	main1
	(export PATH=lib2:$PATH; main2)
	(export PATH=lib3:$PATH; main3)
	(export PATH=lib2:$PATH; main4)
	main5

force:
