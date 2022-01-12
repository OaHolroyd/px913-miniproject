# compiler and linker
FC=gfortran
LD=$(FC)

# flags and libraries
FFLAGS=$(shell nf-config --fflags) -Wall -Wextra -std=f2008
FLIBS=$(shell nf-config --flibs)

# executable names
EXE=particle
EXE_TEST=test

# directories
SRC_DIR=./src
OBJ_DIR=./obj
OUT_DIR=./out

# differentiate between test and particle builds
SRC_SUB=$(filter-out $(SRC_DIR)/$(EXE).f90 $(SRC_DIR)/$(EXE_TEST).f90, $(wildcard $(SRC_DIR)/*.f90))
SRC=$(SRC_SUB) $(SRC_DIR)/$(EXE).f90
SRC_TEST=$(SRC_SUB) $(SRC_DIR)/$(EXE_TEST).f90
OBJ=$(addprefix $(OBJ_DIR)/, $(notdir $(SRC:.f90=.o)))
OBJ_TEST=$(addprefix $(OBJ_DIR)/, $(notdir $(SRC_TEST:.f90=.o)))


# might need to use $(LD) $(FFLAGS) -o $(EXE) $(OBJ) $(FLIBS) #THIS WAS REQUIRED
particle: directories $(OBJ)
	@printf "`tput bold``tput setaf 2`Linking`tput sgr0`\n"
	$(LD) $(FFLAGS) -o $(EXE) $(OBJ) $(FLIBS)
	
# Build rule for binaries (puts .mod files in SRC_DIR to simplify linting)
$(OBJ_DIR)/%.o: $(SRC_DIR)/%.f90
	@printf "`tput bold``tput setaf 6`Building %s`tput sgr0`\n" $@
	$(FC) -J$(SRC_DIR) $(FFLAGS) -c -o $@ $< $(FLIBS)

# build the test file
test: directories $(OBJ_TEST)
	@printf "`tput bold``tput setaf 2`Linking`tput sgr0`\n"
	$(LD) -o $(EXE_TEST) $(OBJ_TEST)

# create required directories
.PHONY: directories
directories:
	mkdir -p $(OBJ_DIR) $(OUT_DIR)

# removes binaries, outputs etc.
.PHONY: clean
clean:
	rm -f $(EXE) $(EXE_TEST) $(OBJ_DIR)/*.o $(SRC_DIR)/*.mod $(OUT_DIR)/**

# force rebuild of all files
.PHONY: all
all: clean particle

# dependencies
$(OBJ_DIR)/model_data.o : $(OBJ_DIR)/create_axis.o $(OBJ_DIR)/command_line.o
$(OBJ_DIR)/gauss_seidel.o : $(OBJ_DIR)/model_data.o
$(OBJ_DIR)/write_netcdf.o : $(OBJ_DIR)/model_data.o
$(OBJ_DIR)/velocity_verlet.o : $(OBJ_DIR)/model_data.o
$(OBJ_DIR)/model_data.o : $(OBJ_DIR)/create_axis.o $(OBJ_DIR)/command_line.o
