all: ym_cpu_comparison 

ym_cpu_comparison:  main.f90 ylmr2.f90 ylmr2_new.f90 
	pgf90 -O3 -mp -Mpreprocess   -Minfo main.f90 ylmr2.f90 ylmr2_new.f90 -o ym_cpu_comparison

clean:
	rm ym_cpu_comparison *.o *.mod

