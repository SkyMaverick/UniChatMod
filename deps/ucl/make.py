import os, sys, subprocess, shutil

from buildsystems import *
from colorize import *

path_return = os.path.abspath (os.path.curdir)
path_current = os.path.abspath(os.path.dirname(__file__))
file_make = os.path.join (path_current, 'Makefile')
file_configure = os.path.join (path_current, 'configure')

def configure_proc (args):
    cmd = ' '.join ([file_configure] + args)
    hint (cmd)

    envs = os.environ.copy()
    envs["CFLAGS"] = '-std=gnu89'
    
    proc = subprocess.Popen (cmd, env=envs, shell=True)
    return proc.wait()

def job (path_target, args):
    info ('Build dependency: {dep_name}'.format (dep_name='ucl'))
    if os.path.exists(file_configure):
        os.chdir (path_current)
        if (configure_proc (args) == 0) and os.path.exists (file_make):
            if bs_make (path_current, []) == 0:
                bs_result (path_current, path_target)
        else:
            error ('Make dependency error')
        os.chdir (path_return)
    else:
        error ('Don\'t found Makefile')
