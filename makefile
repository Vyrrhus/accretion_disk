F90=gfortran

t: t.f90 
	${F90} -c module_input.f90
	${F90} -c module_constantes.f90
	${F90} -c t.f90
	${F90} -o t t.o module_input.o module_constantes.o
	
clean:
	rm -f *.o t *.txt
