all: dgemm_gpu

dgemm_cpu: dgemm_test.F90 timer.o
	pgf90 dgemm_test.f90 timer.o -o dgemm_cpu -lblas

dgemm_gpu: dgemm_test.F90 timer.o
	pgf90 -DUSE_CUDA -Mcuda=cc60,cuda8.0 -Mcudalib=cublas dgemm_test.F90 timer.o -o dgemm_gpu


timer.o: timer.c
	gcc -c timer.c
