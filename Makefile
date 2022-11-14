################################################
#	COMPILER
################
F90    := gfortran
FFLAGS := 

# SOURCES, BIN, EXE
SRCDIR  := src
BINDIR  := bin
EXE     := disk
SOURCES := $(shell find $(SRCDIR) -name '*.f90')
OBJECTS := $(patsubst $(SRCDIR)/%,$(BINDIR)/%,$(SOURCES:.f90=.o))
FFLAGS  += -J $(BINDIR)

ifneq ($(BINDIR),)
  $(shell test -d $(BINDIR) || mkdir -p $(BINDIR))
endif

# CREATE OBJS
$(BINDIR)/%.o: $(SRCDIR)/%.f90
	$(F90) $(FFLAGS) -c -o $@ $<

# CREATE EXE
all: $(EXE)

$(EXE): $(OBJECTS)
	$(F90) $(FFLAGS) -o $@ $(OBJECTS)

# CLEAN
clean:
	@rm -rf $(BINDIR)/*.o $(BINDIR)/*.mod
	@rm -rf $(EXE)
	@echo "All binaries and exe deleted"

# DOCUMENTATION
docu:
	@ford documentation.md