F90=gfortran

all:main

#module compile

module_constantes.o : module_constantes.f90
	${F90} -c module_constantes.f90
module_input.o : module_input.f90
	${F90} -c module_input.f90
	
module_declarations.o : module_declarations.f90 module_constantes.o module_input.o
	${F90} module_constantes.o module_input.o -c module_declarations.f90

main : main.f90 module_declarations.o module_constantes.o module_input.o
	${F90} -o main  module_declarations.o module_constantes.o module_input.o
	
clean:
	rm -f *.o main *.txt
