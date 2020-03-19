a.out: driver.c compiler-output.s
	gcc compiler-output.s driver.c

.PHONY: run
run: a.out
	./a.out
