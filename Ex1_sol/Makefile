all: example_vector_add \
     example2_vector_add \
     example3_vector_add \
     example4_vector_add 

example_vector_add: main.f90 vector_add.f90
	pgf90 -O3 main.f90 vector_add.f90 -o example_vector_add

example2_vector_add: main2.f90 vector_add2.f90
	pgf90 -O3 -Minfo  -Mcuda=cc3x main2.f90 vector_add2.f90 -o example2_vector_add

example3_vector_add: main3.f90 vector_add3.f90
	pgf90 -O3 -Minfo  -Mcuda=cc3x vector_add3.f90 main3.f90 -o example3_vector_add

example4_vector_add: vector_add4.o main4.f90 
	pgf90 -O3 -Minfo  -Mcuda=cc3x,cuda8.0 main4.f90 vector_add4.o  -o example4_vector_add

vector_add4.o: vector_add4.f90
	pgf90 -O3 -Mpreprocess -c vector_add4.f90 -o vector_add4_cpu.o 
	pgf90 -O3 -Mcuda=cc3x,cuda8.0 -Mpreprocess -DUSE_CUDA -c vector_add4.f90 -o vector_add4_gpu.o 
	ld  -r  vector_add4_cpu.o vector_add4_gpu.o  -o vector_add4.o
	rm vector_add4_cpu.o vector_add4_gpu.o

clean:
	rm *.o *.mod example_vector_add example2_vector_add  example3_vector_add example4_vector_add 
