# GCC
FC      = gfortran
FCFLAGS = -Og -g -Wall -fimplicit-none -fcheck=all -fbacktrace -Wno-maybe-uninitialized -ffree-form -ffree-line-length-none -fdefault-real-8 -std=f2018 -cpp -D UNIT_TESTING

# configuration
SRCDIR = ../source
OBJDIR = test_builds
OBJLOC = ../build
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
all : run_all_tests.exe run_test_text_inputs_str2num.exe run_test_utils_find_those_alive.exe run_test_utils_histcounts.exe run_test_utils_unit_vec.exe

run_all_tests.exe : run_all_tests.f90 $(B)run_all_tests.obj
	$(FC) $(FCFLAGS) -o run_all_tests.exe run_all_tests.f90 -I$(OBJDIR) -I$(OBJLOC) $(B)fruit.obj $(B)test_text_inputs_str2num.obj $(B)test_utils_find_those_alive.obj $(B)test_utils_histcounts.obj $(B)test_utils_unit_vec.obj $(addprefix $(OBJLOC)/,$(OBJS))

run_test_text_inputs_str2num.exe : run_test_text_inputs_str2num.f90 $(B)run_test_text_inputs_str2num.obj
	$(FC) $(FCFLAGS) -o run_test_text_inputs_str2num.exe run_test_text_inputs_str2num.f90 -I$(OBJDIR) -I$(OBJLOC) $(B)fruit.obj $(B)test_text_inputs_str2num.obj $(addprefix $(OBJLOC)/,$(OBJS))

run_test_utils_find_those_alive.exe : run_test_utils_find_those_alive.f90 $(B)run_test_utils_find_those_alive.obj
	$(FC) $(FCFLAGS) -o run_test_utils_find_those_alive.exe run_test_utils_find_those_alive.f90 -I$(OBJDIR) -I$(OBJLOC) $(B)fruit.obj $(B)test_utils_find_those_alive.obj $(addprefix $(OBJLOC)/,$(OBJS))

run_test_utils_histcounts.exe : run_test_utils_histcounts.f90 $(B)run_test_utils_histcounts.obj
	$(FC) $(FCFLAGS) -o run_test_utils_histcounts.exe run_test_utils_histcounts.f90 -I$(OBJDIR) -I$(OBJLOC) $(B)fruit.obj $(B)test_utils_histcounts.obj $(addprefix $(OBJLOC)/,$(OBJS))

run_test_utils_unit_vec.exe : run_test_utils_unit_vec.f90 $(B)run_test_utils_unit_vec.obj
	$(FC) $(FCFLAGS) -o run_test_utils_unit_vec.exe run_test_utils_unit_vec.f90 -I$(OBJDIR) -I$(OBJLOC) $(B)fruit.obj $(B)test_utils_unit_vec.obj $(addprefix $(OBJLOC)/,$(OBJS))

# object file implicit rules
$(B)%.obj : %.f90
	$(FC) -c $(FCFLAGS) -J$(OBJDIR) -I$(OBJDIR) -I$(OBJLOC) -o $@ $<

# object file dependencies
$(B)fruit.obj : fruit.f90

$(B)run_all_tests.obj : run_all_tests.f90 $(B)fruit.obj $(B)test_text_inputs_str2num.obj $(B)test_utils_find_those_alive.obj $(B)test_utils_histcounts.obj $(B)test_utils_unit_vec.obj

$(B)run_test_text_inputs_str2num.obj : run_test_text_inputs_str2num.f90 $(B)fruit.obj $(B)test_text_inputs_str2num.obj

$(B)run_test_utils_find_those_alive.obj : run_test_utils_find_those_alive.f90 $(B)fruit.obj $(B)test_utils_find_those_alive.obj

$(B)run_test_utils_histcounts.obj : run_test_utils_histcounts.f90 $(B)fruit.obj $(B)test_utils_histcounts.obj

$(B)run_test_utils_unit_vec.obj : run_test_utils_unit_vec.f90 $(B)fruit.obj $(B)test_utils_unit_vec.obj

$(B)test_text_inputs_str2num.obj : test_text_inputs_str2num.f90 $(B)fruit.obj

$(B)test_utils_find_those_alive.obj : test_utils_find_those_alive.f90 $(B)fruit.obj

$(B)test_utils_histcounts.obj : test_utils_histcounts.f90 $(B)fruit.obj

$(B)test_utils_unit_vec.obj : test_utils_unit_vec.f90 $(B)fruit.obj

# clean-up
clean :
	$(RM) $(B)*.obj $(B)*.mod $(B)*.smod run_test_*.exe

.PHONY : clean all
