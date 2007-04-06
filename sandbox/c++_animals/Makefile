CPP_OPT= -mno-cygwin
EXT=.exe
LIBGNAT_DIR=c:/my_home/local/lib/gcc/pentium-mingw32msv/4.1.3/rts-sjlj/adalib
STDCPP_DIR=/cygwin/lib/gcc/i686-pc-mingw32/3.4.4
RTS=--RTS=sjlj

auto:
	gprbuild animals.gpr
work:
	g++ -c $(CPP_OPT) animals_c.cc
	g++ $(CPP_OPT) -r animals_c.o -nostdlib -o animals_c.oo -lstdc++ -lgcc
	gnatmake -c  $(RTS) main.adb -gnat05
	gnatbind $(RTS) main
	gcc -c -gnat05 $(RTS) b~main.adb
	gcc  animals_c.oo  b~main.o animals.o main.o -o main$(EXT) $(LIBGNAT_DIR)/libgnat.a -static-libgcc

dont_work:
	g++ -c $(CPP_OPT) animals_c.cc
	gnatmake -c  $(RTS) main.adb -gnat05
	gnatbind $(RTS) main
	gcc -c -gnat05 $(RTS) b~main.adb
	gcc  animals_c.o $(STDCPP_DIR)/libstdc++.a $(STDCPP_DIR)/libgcc.a  b~main.o animals.o main.o -o main$(EXT) $(LIBGNAT_DIR)/libgnat.a -static-libgcc

clean:
	gnatclean main
	$(RM) *~ *.o *.oo 