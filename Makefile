################################################
#	COMPILER
################
F90	   := gfortran
FFLAGS := -g -O2 -fno-second-underscore -falign-loops=16 -fno-ipa-cp-clone
#FFLAGS := -g -Og -fno-second-underscore -Wall -Wextra -Wno-compare-reals -fcheck=all -fimplicit-none -std=f2008 -pedantic -ffpe-trap=invalid,zero,overflow -fbacktrace -fall-intrinsics -Wno-unused-function -fdump-core -ggdb -finit-real=nan -finit-logical=false -finit-integer=999999 -finit-derived
#FFLAGS :=-g -Og -fno-second-underscore -Wall -Wextra -Wno-compare-reals -fcheck=all -fimplicit-none -std=f2008 -pedantic -ffpe-trap=invalid,zero,overflow -fbacktrace -fall-intrinsics -Wno-unused-function -fdump-core -ggdb -Wuninitialized
LIB  = -llapack -lblas -lpthread

# DIRECTORIES
SRCDIR     := src
BINDIR     := bin
CONFIGDIR  := config
DEFAULTDIR := default
VPATH := $(SRCDIR) $(dir $(wildcard $(SRCDIR)/*/.))

# SOURCES
MODULE_SRC 	:= 	modules/declarations.f90 \
                modules/dimensionnement.f90 \
                modules/ecriture.f90 \
                modules_scurve/scurve_utils.f90 \
                modules_scurve/dichotomie.f90 \
                modules_scurve/scurve.f90 \
                modules/equations.f90 \
                modules/conditions_initiales.f90 \
                modules/frames2D.f90 \
                modules_schemas/schemas_temp.f90 \
                modules_schemas/schemas_sigma.f90 \
                modules_schemas/schemas_instabilite.f90 \
                modules/boucles.f90

MAIN_SRC 	:= main.f90

# EXECUTABLES
MAIN_EXE := disk

# CREATE OBJECTS
MODULE_SRC := $(foreach file,$(MODULE_SRC),$(SRCDIR)/$(file))
MAIN_SRC   := $(foreach file,$(MAIN_SRC),$(SRCDIR)/$(file))

MODULE_OBJ := $(patsubst %,$(BINDIR)/%,$(notdir $(MODULE_SRC:.f90=.o)))
MAIN_OBJ   := $(patsubst %,$(BINDIR)/%,$(notdir $(MAIN_SRC:.f90=.o)))

FFLAGS  += -J $(BINDIR)
ifneq ($(BINDIR),)
$(shell test -d $(BINDIR) || mkdir -p $(BINDIR))
endif

################################################
#	CONFIG FILES
################
$(shell cp -n $(DEFAULTDIR)/input.config $(CONFIGDIR)/input.config)
$(shell cp -n $(DEFAULTDIR)/output.config $(CONFIGDIR)/output.config)
$(shell cp -n $(DEFAULTDIR)/conditions_initiales.config $(CONFIGDIR)/conditions_initiales.config)
$(shell cp -n $(DEFAULTDIR)/wanted_variables.txt $(CONFIGDIR)/wanted_variables.txt)

################################################
#	TARGETS
################

# PROGRAMS
default: $(MAIN_EXE)

all: $(MAIN_EXE) $(SCURVE_EXE)

$(MAIN_EXE): $(MODULE_OBJ) $(MAIN_OBJ)
	$(F90) $(FFLAGS) $^ -o $@ $(LIB)

$(BINDIR)/%.o: %.f90
	$(F90) $(FFLAGS) -c -o $@ $<

# CLEAN
clean:
	@rm -rf $(BINDIR)/*.o $(BINDIR)/*.mod
	@rm -rf disk schema
	@echo "All binaries and exe deleted"

# DOCUMENTATION
docu:
	@ford documentation.md
