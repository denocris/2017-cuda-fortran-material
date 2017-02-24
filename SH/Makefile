all: ym_cpu 

ym_cpu:  main.f90 ylmr2.f90 timer.o
	pgf90 -O3 -mp   -Minfo main.f90 ylmr2.f90 timer.o  -o ym_cpu

timer.o: timer.c
	gcc -c timer.c
clean:
	rm ym_cpu *.o

