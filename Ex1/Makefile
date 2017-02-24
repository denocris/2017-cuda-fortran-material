all: example_vector_add 

example_vector_add: main.f90 vector_add.f90
	pgf90 -O3 main.f90 vector_add.f90 -o example_vector_add

clean:
	rm *.o *.mod example_vector_add 
