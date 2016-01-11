#==============================================================================
#
# Makefile for PCRTM example program
#
#==============================================================================

# Macro definitions
# -----------------
# The location of the PCRTM library
# *** CHANGE THIS TO THE LOCATION ON YOUR SYSTEM ***
PCRTM_DIR = ../GENERIC/

# The Fortran95 compiler+flags, and linker+flags
# *** CHANGE THESE FOR THE COMPILER YOU USE ON YOUR SYSTEM ***
INTEL = ifort
I_OPTIM = -O3\
	   -assume byterecl\
	   -c \
           -I$(PCRTM_DIR)/include\

I_DEBUG = -g\
	   -assume byterecl\
	   -c \
	   -check all \
           -traceback \
           -warn errors \
           -free \
           -I$(PCRTM_DIR)/include\


GFORTRAN = gfortran
G_DEBUG = -c \
           -fbounds-check \
           -ffpe-trap=overflow,zero \
           -ffree-form \
           -fno-second-underscore \
           -frecord-marker=4 \
           -static \
           -Wall \
          -I$(PCRTM_DIR)/include\

G_OPTIM  = -c \
           -O3\
           -ffast-math \
           -ffree-form \
           -fno-second-underscore \
           -frecord-marker=4 \
           -funroll-loops \
           -static \
           -Wall \
           -I$(PCRTM_DIR)/include\

#FC = $(INTEL)
#FC_FLAG = $(I_DEBUG)
FC = $(GFORTRAN)
FC_FLAG = $(G_DEBUG)


FL = $(FC)
FL_FLAGS = -L$(PCRTM_DIR)/lib -lPCRTM -o

# The executable and object files
EXE_FILE =  FWD_simu_sample.exe
OBJ_FILE =  $(EXE_FILE:.exe=.o)


# Target definitions
# ------------------
# The main target
all: $(OBJ_FILE)
	$(FL) $(OBJ_FILE) $(FL_FLAGS) ./$(EXE_FILE)

# A clean target
clean:
	rm -f $(OBJ_FILE) ./$(EXE_FILE)

# Local dependencies
$(EXE_FILE:.exe=.o): $(EXE_FILE:.exe=.f90) 


# Suffix rule definitions
# -----------------------
.SUFFIXES:
.SUFFIXES: .f90 .o
.f90.o:
	$(FC) $(FC_FLAG) $<
