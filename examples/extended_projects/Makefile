all: ../subsystems/default.cgpr
	gprbuild -Pnew_ada_main --config=../subsystems/default.cgpr

../subsystems/default.cgpr:
	make -C ../subsystems default.cgpr

clean:
	gprclean -r -Pnew_ada_main --config=../subsystems/default.cgpr


run: all
	./ada_main
