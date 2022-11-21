################################################
#	COMPILER
################
F90    := gfortran
FFLAGS := 

# DIRECTORIES
SRCDIR  := src
BINDIR  := bin

# SOURCES
MODULE_SRC 	:= 	module_declarations.f90 \
				module_fonctions_utiles.f90 \
				module_function.f90 \
				module_conditions_initiales.f90 \
				# module_dicho.f90 \
			    # 
				# module_branche_epais.f90

MAIN_SRC 	:= main.f90
SCURVE_SRC 	:= S_curve.f90

# EXECUTABLES
MAIN_EXE := disk
SCURVE_EXE := scurve

# CREATE OBJECTS
MODULE_SRC := $(foreach file,$(MODULE_SRC),$(SRCDIR)/$(file))
MAIN_SRC   := $(foreach file,$(MAIN_SRC),$(SRCDIR)/$(file))
SCURVE_SRC := $(foreach file,$(SCURVE_SRC),$(SRCDIR)/$(file))

MODULE_OBJ := $(patsubst $(SRCDIR)/%,$(BINDIR)/%,$(MODULE_SRC:.f90=.o))
MAIN_OBJ   := $(patsubst $(SRCDIR)/%,$(BINDIR)/%,$(MAIN_SRC:.f90=.o))
SCURVE_OBJ := $(patsubst $(SRCDIR)/%,$(BINDIR)/%,$(SCURVE_SRC:.f90=.o))

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

$(MAIN_EXE): $(MODULE_OBJ) $(MAIN_OBJ)
	$(F90) $(FFLAGS) $^ -o $@

$(SCURVE_EXE): $(MODULE_OBJ) $(SCURVE_OBJ)
	$(F90) $(FFLAGS) $^ -o $@

$(BINDIR)/%.o: $(SRCDIR)/%.f90
	$(F90) $(FFLAGS) -c -o $@ $<

# CLEAN
clean:
	@rm -rf $(BINDIR)/*.o $(BINDIR)/*.mod
	@rm -rf $(EXE)
	@echo "All binaries and exe deleted"

# DOCUMENTATION
docu:
	@ford documentation.md
