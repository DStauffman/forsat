# GCC
FC      = gfortran
FCFLAGS = -O3 -ffree-form -ffree-line-length-none -fdefault-real-8 -std=f2018 -cpp
DBFLAGS = -Og -g -Wall -fimplicit-none -fcheck=all -fbacktrace -Wno-maybe-uninitialized
DBFLAGS =
# Intel (also need to change build rule below)
#FC      = ifort
#FCFLAGS = -O3 -standard-semantics -fpp -D SKIP_ERR_STOP_MSG -D SKIP_ASSERTS -D OLD_IFORT
#DBFLAGS = -traceback -check bounds -check uninit -standard-semantics

# configuration
SRCDIR = source
OBJDIR = build
OBJS   = \
       asserts.obj \
       constants.obj \
       enums.obj \
       logging.obj \
       matlab.obj \
       operators.obj \
       parameters.obj \
       prng_nums.obj \
       stats.obj \
       text_input.obj \
       text_output.obj \
       utils.obj \

# no implicit rules
.SUFFIXES:

# auxiliary programs
RM:=rm -f
MKDIR:=mkdir -p
TEST:=test -d

# create the build directory; define slashed version for convenience with short names
ifneq ($(OBJDIR),)
  $(shell $(TEST) $(OBJDIR) || $(MKDIR) $(OBJDIR))
  B:=$(OBJDIR)/
else
  B:=./
endif
ifneq ($(SRCDIR),)
  S:=$(SRCDIR)/
else
  S:=./
endif

# main executable
all : forsat

forsat : $(S)forsat.f90 $(B)forsat.obj
	$(FC) $(FCFLAGS) $(DBFLAGS) $(FPPFLAGS) -o forsat_debug.exe $(S)forsat.f90 -I$(OBJDIR) $(addprefix $(B),$(OBJS))

# object file implicit rules
$(B)%.obj : $(S)%.f90
	$(FC) -c $(FCFLAGS) $(DBFLAGS) $(FPPFLAGS) -J$(OBJDIR) -I$(OBJDIR) -o $@ $<
#	$(FC) -c $(FCFLAGS) $(DBFLAGS) $(FPPFLAGS) -module $(OBJDIR) -o $@ $<

# object file dependencies
$(B)asserts.obj : $(S)asserts.f90 $(B)text_output.obj

$(B)constants.obj : $(S)constants.f90

$(B)enums.obj : $(S)enums.f90

#$(B)forsat.obj : $(S)forsat.f90 $(B)constants.obj
$(B)forsat.obj : $(S)forsat.f90 $(addprefix $(B),$(OBJS))

$(B)logging.obj : $(S)logging.f90 $(B)constants.obj

$(B)matlab.obj : $(S)matlab.f90 $(B)constants.obj

$(B)operators.obj : $(S)operators.f90 $(B)constants.obj

$(B)parameters.obj : $(S)parameters.f90 $(B)asserts.obj $(B)constants.obj $(B)logging.obj $(B)text_input.obj $(B)text_output.obj $(B)utils.obj

$(B)prng_nums.obj : $(S)prng_nums.f90 $(B)constants.obj $(B)matlab.obj $(B)text_output.obj

$(B)stats.obj : $(S)stats.f90 $(B)constants.obj

$(B)text_input.obj : $(S)text_input.f90 $(B)asserts.obj $(B)constants.obj

$(B)text_output.obj : $(S)text_output.f90 $(B)constants.obj $(B)logging.obj $(B)matlab.obj

$(B)utils.obj : $(S)utils.f90 $(B)asserts.obj $(B)constants.obj $(B)matlab.obj $(B)prng_nums.obj $(B)operators.obj $(B)text_output.obj

# clean-up
.PHONY : all clean forsat
clean :
	$(RM) $(B)*.obj $(B)*.mod $(B)*.smod forsat*.exe
	$(TEST) -d $(OBJDIR) && $(RM) -r $(OBJDIR)
