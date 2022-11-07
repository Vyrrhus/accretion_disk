F90=gfortran

.MAKEOPTS: -k -s

.SUFFIXES: .f90

OBJdisque = module_declarations.o 

all: main.o main
	echo "Compilation finished"

#module compile

%.o : %.f90
	$(F90) -c $<

main.o : main.f90
	$(F90) -c main.f90

main : $(OBJdisque) main.f90
	${F90} main.o $(OBJdisque) -o main
	
clean:
	rm -f *.mod *.o main
