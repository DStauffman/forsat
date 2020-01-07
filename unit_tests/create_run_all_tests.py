# -*- coding: utf-8 -*-
r"""
Script to autogenerate the unit test programs.

Notes
-----
#.  Written by David C. Stauffer in December 2019.

"""

#%% Imports
import os
from dstauffman import create_fortran_unit_tests, get_root_dir

#%% Makefile template
template = \
r"""# GCC
FC      = gfortran
FCFLAGS = -Og -g -Wall -fimplicit-none -fcheck=all -fbacktrace -Wno-maybe-uninitialized -ffree-form -ffree-line-length-none -fdefault-real-8 -std=f2018 -cpp -D UNIT_TESTING

# configuration
SRCDIR = ../source
OBJDIR = test_builds
OBJLOC = ../build
OBJS   = \

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

# object file implicit rules
$(B)%.obj : %.f90
	$(FC) -c $(FCFLAGS) -J$(OBJDIR) -I$(OBJDIR) -I$(OBJLOC) -o $@ $<

# object file dependencies

# clean-up
clean :
	$(RM) $(B)*.obj $(B)*.mod $(B)*.smod run_*.exe

.PHONY : clean all
"""

#%% Constants
folder = os.path.abspath(os.path.join(get_root_dir(), '..', '..', 'forsat', 'unit_tests'))
external_sources = {'asserts', 'constants', 'enums', 'logging', 'matlab', 'operators', \
    'parameters', 'prng_nums', 'quaternions', 'stats', 'text_input', 'text_output', 'utils', \
    'vectors'}

if __name__ == '__main__':
    create_fortran_unit_tests(folder, template=template, external_sources=external_sources)
