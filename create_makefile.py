r"""
Script to autogenerate the makefile.

Notes
-----
#.  Written by David C. Stauffer in January 2020.
"""

#%% Imports
import glob
import os
from dstauffman import create_fortran_makefile, get_root_dir

#%% Configuration
# user settings
#folder   = os.path.join(os.path.realpath(os.path.join(os.getcwd(), os.path.dirname(__file__))), 'source')
folder   = os.path.abspath(os.path.join(get_root_dir(), '..', '..', 'forsat', 'source'))
program  = 'forsat'
compiler = 'gfortran'
is_debug = True
# Derived constants
temp     = glob.glob(os.path.join(folder, '*.f90'))
sources  = sorted([os.path.basename(x).split('.')[0] for x in temp])
build    = 'debug' if is_debug else 'release'
#makefile = os.path.abspath(os.path.join(folder, '..', program + '_' + compiler + '_' + build + '.make'))
makefile = os.path.abspath(os.path.join(folder, '..', program + '.make'))

#%% Makefile template
# compiler flags
fcflags = dict()
fcflags['gfortran'] = '-O3 -ffree-form -ffree-line-length-none -fdefault-real-8 -std=f2018 -cpp'
fcflags['ifort']    = '-O3 -standard-semantics -fpp'
fcflags['win']      = '/O3 /free /extend-source:132 /real-size:64 /Qm64 /standard-semantics /fpp /define:SKIP_ASSERTS /define:OLD_IFORT'
# debug flags
dbflags = dict()
dbflags['gfortran'] = '-Og -g -Wall -fimplicit-none -fcheck=all -fbacktrace -Wno-maybe-uninitialized'
dbflags['ifort']    = '-traceback -check bounds -check uninit -standard-semantics'
dbflags['win']      = '/warn:all /traceback /check:bounds /check:uninit'
# makefile template
template = \
'# compiler and flags\n' + \
'FC      = ' + (compiler if compiler != 'win' else 'ifort') + '\n' + \
'FCFLAGS = ' + fcflags[compiler] + '\n' + \
'DBFLAGS = ' + (dbflags[compiler] if is_debug else '') + r"""

# configuration
SRCDIR = source
OBJDIR = build
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
$(B)%.obj : $(S)%.f90
	$(FC) -c $(FCFLAGS) $(DBFLAGS) $(FPPFLAGS) -J$(OBJDIR) -I$(OBJDIR) -o $@ $<

# object file dependencies

# clean-up
.PHONY : all clean forsat
clean :
	$(RM) $(B)*.obj $(B)*.mod $(B)*.smod forsat.exe
	$(TEST) -d $(OBJDIR) && $(RM) -r $(OBJDIR)

"""

#%% Script
if __name__ == '__main__':
    create_fortran_makefile(folder, makefile, template=template, program=program, sources=sources, \
                            compiler=compiler, is_debug=is_debug)
