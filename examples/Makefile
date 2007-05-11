all:
	-${foreach dir,${wildcard */}, make all -C ${dir};}
clean: force
	-${foreach dir,${wildcard */}, make clean -C ${dir};}
run:
	-${foreach dir,${wildcard */}, make run -C ${dir};}
force:
