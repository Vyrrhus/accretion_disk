F90=gfortran

.MAKEOPTS: -k -s

.SUFFIXES: .f90

OBJdisque = module_declarations.o 

all: main.o main
	echo "Compilation finished"

#module compile

#module_constantes.o : module_constantes.f90
#	${F90} -c module_constantes.f90
#module_input.o : module_input.f90
#	${F90} -c module_input.f90
#	
#module_declarations.o : module_declarations.f90 module_constantes.o module_input.o
#	${F90} module_constantes.o module_input.o -c module_declarations.f90

%.o : %.f90
	$(F90) -c $<

main.o : main.f90
	$(F90) -c main.f90

main : $(OBJdisque) main.f90
	${F90} main.o $(OBJdisque) -o main
	
clean:
	rm -f *.o main
