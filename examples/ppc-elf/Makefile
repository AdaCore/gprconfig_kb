all: objs/hello

objs/hello: hello.cgpr force
	gprbuild --config=hello.cgpr -Phello

hello.cgpr:
	gprconfig -batch -config GNAT,,,Ada,powerpc-elf,zfp -config GCC-ASM,,,Asm,powerpc-elf, -config GCC,,,C,powerpc-elf, -o $@

clean: force
	gprclean --config=hello.cgpr -Phello
	$(RM) hello.cgpr

run: all
	powerpc-elf-gdb --batch --command=gdb.run objs/hello

force:
