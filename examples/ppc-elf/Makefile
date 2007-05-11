objs/hello: hello.cgpr force
	gprbuild --config=hello.cgpr -Phello 

hello.cgpr:
	gprconfig -batch -config GNAT,,,Ada,powerpc-elf,zfp -config GCC-ASM,,,Asm,powerpc-elf, -config GCC,,,C,powerpc-elf, -o $@


all: obj/hello

clean: force
	gprclean --config=hello.cgpr -Phello
	$(RM) hello.cgpr

run:
	powerpc-elf-gdb --batch --command=gdb.run objs/hello


force:
