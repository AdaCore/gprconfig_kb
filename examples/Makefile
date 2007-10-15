PTH := .:$(PATH)
export PATH=${PTH}

all clean run:
	-${foreach dir,${wildcard */}, ${MAKE} $@ -C ${dir};}
.PHONY: all clean run
