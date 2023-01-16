################################################
#	COMPILER
################
F90	   := gfortran
FFLAGS := -g -O2 -fno-second-underscore -falign-loops=16 -fno-ipa-cp-clone
#FFLAGS := -g -Og -fno-second-underscore -Wall -Wextra -Wno-compare-reals -fcheck=all -fimplicit-none -std=f2008 -pedantic -ffpe-trap=invalid,zero,overflow -fbacktrace -fall-intrinsics -Wno-unused-function -fdump-core -ggdb -finit-real=nan -finit-logical=false -finit-integer=999999 -finit-derived
#FFLAGS :=-g -Og -fno-second-underscore -Wall -Wextra -Wno-compare-reals -fcheck=all -fimplicit-none -std=f2008 -pedantic -ffpe-trap=invalid,zero,overflow -fbacktrace -fall-intrinsics -Wno-unused-function -fdump-core -ggdb -Wuninitialized
LIB  = -llapack -lblas -lpthread

# DIRECTORIES
SRCDIR  := src
BINDIR  := bin

# SOURCES
MODULE_SRC 	:= 	module_declarations.f90 \
				dimensionnement.f90 \
				module_fonctions_utiles.f90 \
				module_function.f90 \
				module_ecriture.f90 \
				module_dicho.f90 \
				module_conditions_initiales.f90 \
				module_schemas_sigma.f90 \
				module_schemas_T.f90 \
				module_s_curve.f90 \
				module_boucle.f90

MAIN_SRC 	:= main.f90
SCURVE_SRC 	:= S_curve.f90
SCHEMA_SRC  := program_schema.f90

# EXECUTABLES
MAIN_EXE := disk
SCURVE_EXE := scurve
SCHEMA_EXE := schema

# CREATE OBJECTS
MODULE_SRC := $(foreach file,$(MODULE_SRC),$(SRCDIR)/$(file))
MAIN_SRC   := $(foreach file,$(MAIN_SRC),$(SRCDIR)/$(file))
SCURVE_SRC := $(foreach file,$(SCURVE_SRC),$(SRCDIR)/$(file))
SCHEMA_SRC := $(foreach file,$(SCHEMA_SRC),$(SRCDIR)/$(file))

MODULE_OBJ := $(patsubst $(SRCDIR)/%,$(BINDIR)/%,$(MODULE_SRC:.f90=.o))
MAIN_OBJ   := $(patsubst $(SRCDIR)/%,$(BINDIR)/%,$(MAIN_SRC:.f90=.o))
SCURVE_OBJ := $(patsubst $(SRCDIR)/%,$(BINDIR)/%,$(SCURVE_SRC:.f90=.o))
SCHEMA_OBJ := $(patsubst $(SRCDIR)/%,$(BINDIR)/%,$(SCHEMA_SRC:.f90=.o))

FFLAGS  += -J $(BINDIR)
ifneq ($(BINDIR),)
$(shell test -d $(BINDIR) || mkdir -p $(BINDIR))
endif

################################################
#	TARGETS
################

# PROGRAMS
default: $(MAIN_EXE)

all: $(MAIN_EXE) $(SCURVE_EXE)

curve: $(SCURVE_EXE)

schema: $(SCHEMA_EXE)

$(MAIN_EXE): $(MODULE_OBJ) $(MAIN_OBJ)
	$(F90) $(FFLAGS) $^ -o $@ $(LIB)

$(SCURVE_EXE): $(MODULE_OBJ) $(SCURVE_OBJ)
	$(F90) $(FFLAGS) $^ -o $@ $(LIB)

$(SCHEMA_EXE): $(MODULE_OBJ) $(SCHEMA_OBJ) 
	$(F90) $(FFLAGS) $^ -o $@ $(LIB)

$(BINDIR)/%.o: $(SRCDIR)/%.f90
	$(F90) $(FFLAGS) -c -o $@ $<

# CLEAN
clean:
	@rm -rf $(BINDIR)/*.o $(BINDIR)/*.mod
	@rm -rf disk scurve schema
	@echo "All binaries and exe deleted"

# DOCUMENTATION
docu:
	@ford documentation.md
