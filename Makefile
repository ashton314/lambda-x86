a.out: driver.c compiler-output.s
	gcc compiler-output.s driver.c

.PHONY: run
run: a.out
	./a.out

.PHONY: test
test:
	raco test compiler.rkt
