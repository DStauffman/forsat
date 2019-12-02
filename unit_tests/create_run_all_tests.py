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

#%% Constants
folder = os.path.abspath(os.path.join(get_root_dir(), '..', '..', 'forsat', 'unit_tests'))

if __name__ == '__main__':
    create_fortran_unit_tests(folder)
