all: default.cgpr
	gprbuild main.gpr -XLIB=static
	gprbuild main.gpr -XLIB=dynamic


default.cgpr:
	gprconfig --batch --config Ada  --config C -o $@

clean: force
	gprclean -r main.gpr -XLIB=static
	gprclean -r main.gpr -XLIB=dynamic
	$(RM) default.cgpr

run: all
	./obj/main
	(PATH="../libraries/lib2:${PATH}" obj1/main)

force:
