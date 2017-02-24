all: ym_cpu_comparison \
     ym_cpu_comparison_scalar \
     ym_gpu_comparison

ym_cpu_comparison:  main.f90 ylmr2.f90 ylmr2_new.f90 
	pgf90 -O3 -mp -Mpreprocess   -Minfo main.f90 ylmr2.f90 ylmr2_new.f90 -o ym_cpu_comparison

ym_cpu_comparison_scalar:  main.f90 ylmr2.f90 ylmr2_scalar.f90 
	pgf90 -O3 -mp -Mpreprocess   -Minfo main.f90 ylmr2.f90 ylmr2_scalar.f90 -o ym_cpu_comparison_scalar

ym_gpu_comparison:  main2.f90 ylmr2.f90 ylmr2_new.f90 
	pgf90 -O3 -mp -Mpreprocess  -DUSE_CUDA -Mcuda=cc60,cuda8.0,ptxinfo -Minfo \
	main2.f90 ylmr2.f90 ylmr2_new.f90 -o ym_gpu_comparison

clean:
	rm ym_cpu_comparison ym_cpu_comparison_scalar ym_gpu_comparison *.o *.mod

