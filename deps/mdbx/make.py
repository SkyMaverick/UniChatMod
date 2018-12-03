import os
       
from buildsystems import *
from colorize import *

path_current = os.path.abspath(os.path.dirname(__file__))
file_make = os.path.join (path_current, 'Makefile')

def job (path_target, args):
    info ('Build dependency: {dep_name}'.format (dep_name='mdbx'))
    if os.path.exists (file_make):
        bs_make (path_current, ['clean'])
        if bs_make (path_current, ['libmdbx.so']) == 0:
            bs_result (path_current, path_target)
        else:
            error ('Make dependency error')
    else:
        error ('Don\'t found Makefile')
