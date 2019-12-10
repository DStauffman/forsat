# GCC
FC      = gfortran
FCFLAGS = -Og -g -Wall -fimplicit-none -fcheck=all -fbacktrace -Wno-maybe-uninitialized -ffree-form -ffree-line-length-none -fdefault-real-8 -std=f2018 -cpp -D UNIT_TESTING

# configuration
SRCDIR = ../source
OBJDIR = test_builds
OBJLOC = ../build
OBJS   = \
       asserts.obj \
       classes.obj \
       constants.obj \
       enums.obj \
       general.obj \
       hiv.obj \
       logging.obj \
       main.obj \
       matlab.obj \
       operators.obj \
       parameters.obj \
       param_config.obj \
       param_hiv.obj \
       param_life.obj \
       prng_nums.obj \
       stats.obj \
       text_input.obj \
       text_output.obj \
       transmission.obj \
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
all : run_test_hiv_update_cd4.exe run_test_hiv_update_vl.exe run_test_utils_bin_by_age.exe run_test_utils_estimate_max_num_people.exe run_test_utils_find_those_alive.exe run_test_utils_histcounts.exe

run_test_hiv_update_cd4.exe : run_test_hiv_update_cd4.f90 $(B)run_test_hiv_update_cd4.obj
	$(FC) $(FCFLAGS) -o run_test_hiv_update_cd4.exe run_test_hiv_update_cd4.f90 -I$(OBJDIR) -I$(OBJLOC) $(B)fruit.obj $(B)test_hiv_update_cd4.obj $(addprefix $(OBJLOC)/,$(OBJS))

run_test_hiv_update_vl.exe : run_test_hiv_update_vl.f90 $(B)run_test_hiv_update_vl.obj
	$(FC) $(FCFLAGS) -o run_test_hiv_update_vl.exe run_test_hiv_update_vl.f90 -I$(OBJDIR) -I$(OBJLOC) $(B)fruit.obj $(B)test_hiv_update_vl.obj $(addprefix $(OBJLOC)/,$(OBJS))

run_test_utils_bin_by_age.exe : run_test_utils_bin_by_age.f90 $(B)run_test_utils_bin_by_age.obj
	$(FC) $(FCFLAGS) -o run_test_utils_bin_by_age.exe run_test_utils_bin_by_age.f90 -I$(OBJDIR) -I$(OBJLOC) $(B)fruit.obj $(B)test_utils_bin_by_age.obj $(addprefix $(OBJLOC)/,$(OBJS))

run_test_utils_estimate_max_num_people.exe : run_test_utils_estimate_max_num_people.f90 $(B)run_test_utils_estimate_max_num_people.obj
	$(FC) $(FCFLAGS) -o run_test_utils_estimate_max_num_people.exe run_test_utils_estimate_max_num_people.f90 -I$(OBJDIR) -I$(OBJLOC) $(B)fruit.obj $(B)test_utils_estimate_max_num_people.obj $(addprefix $(OBJLOC)/,$(OBJS))

run_test_utils_find_those_alive.exe : run_test_utils_find_those_alive.f90 $(B)run_test_utils_find_those_alive.obj
	$(FC) $(FCFLAGS) -o run_test_utils_find_those_alive.exe run_test_utils_find_those_alive.f90 -I$(OBJDIR) -I$(OBJLOC) $(B)fruit.obj $(B)test_utils_find_those_alive.obj $(addprefix $(OBJLOC)/,$(OBJS))

run_test_utils_histcounts.exe : run_test_utils_histcounts.f90 $(B)run_test_utils_histcounts.obj
	$(FC) $(FCFLAGS) -o run_test_utils_histcounts.exe run_test_utils_histcounts.f90 -I$(OBJDIR) -I$(OBJLOC) $(B)fruit.obj $(B)test_utils_histcounts.obj $(addprefix $(OBJLOC)/,$(OBJS))

# object file implicit rules
$(B)%.obj : %.f90
	$(FC) -c $(FCFLAGS) -J$(OBJDIR) -I$(OBJDIR) -I$(OBJLOC) -o $@ $<

# object file dependencies
$(B)fruit.obj : fruit.f90

$(B)run_test_hiv_update_cd4.obj : run_test_hiv_update_cd4.f90 $(B)fruit.obj $(B)test_hiv_update_cd4.obj

$(B)run_test_hiv_update_vl.obj : run_test_hiv_update_vl.f90 $(B)fruit.obj $(B)test_hiv_update_vl.obj

$(B)run_test_utils_bin_by_age.obj : run_test_utils_bin_by_age.f90 $(B)fruit.obj $(B)test_utils_bin_by_age.obj

$(B)run_test_utils_estimate_max_num_people.obj : run_test_utils_estimate_max_num_people.f90 $(B)fruit.obj $(B)test_utils_estimate_max_num_people.obj

$(B)run_test_utils_find_those_alive.obj : run_test_utils_find_those_alive.f90 $(B)fruit.obj $(B)test_utils_find_those_alive.obj

$(B)run_test_utils_histcounts.obj : run_test_utils_histcounts.f90 $(B)fruit.obj $(B)test_utils_histcounts.obj

$(B)test_hiv_update_cd4.obj : test_hiv_update_cd4.f90 $(B)fruit.obj

$(B)test_hiv_update_vl.obj : test_hiv_update_vl.f90 $(B)fruit.obj

$(B)test_utils_bin_by_age.obj : test_utils_bin_by_age.f90 $(B)fruit.obj

$(B)test_utils_estimate_max_num_people.obj : test_utils_estimate_max_num_people.f90 $(B)fruit.obj

$(B)test_utils_find_those_alive.obj : test_utils_find_those_alive.f90 $(B)fruit.obj

$(B)test_utils_histcounts.obj : test_utils_histcounts.f90 $(B)fruit.obj

# clean-up
clean :
	$(RM) $(B)*.obj $(B)*.mod $(B)*.smod run_test_*.exe

.PHONY : clean all
