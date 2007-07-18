all: objs/hello

objs/hello: hello.cgpr force
	gprbuild --config=hello.cgpr -Phello

hello.cgpr:
	gprconfig --batch --target=powerpc-elf --config Ada,,zfp --config Asm --config C -o $@

clean: force
	gprclean --config=hello.cgpr -Phello
	$(RM) hello.cgpr

run: all
	powerpc-elf-gdb --batch --command=gdb.run objs/hello

force:
