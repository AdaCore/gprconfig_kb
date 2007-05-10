objs/hello: hello.cgpr force
	gprbuild --config=hello.cgpr -Phello 

hello.cgpr:
	gprconfig -batch -config GNAT,,,Ada,powerpc-elf,zfp -config GCC-ASM,,,Asm,powerpc-elf, -config GCC,,,C,powerpc-elf, -o $@


PREFIX=powerpc-elf-

# Currently not run by default.
all:

clean:

run:
	powerpc-elf-gdb --batch --command=gdb.run objs/hello


force:
