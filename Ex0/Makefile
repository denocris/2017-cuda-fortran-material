all: dgemm_cpu

dgemm_cpu: dgemm_test.F90 timer.o
	pgf90 dgemm_test.f90 timer.o -o dgemm_cpu -lblas

timer.o: timer.c
	gcc -c timer.c
