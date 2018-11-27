import os, sys, subprocess, shutil
from colorize import info, error

path_current = os.path.abspath(os.path.dirname(__file__))
file_make = os.path.join (path_current, 'Makefile')
file_configure = os.path.join (path_current, 'configure')

def move_proc (path):
    for root, dirs, files in os.walk (path_current):
        for file in files:
            if file.endswith('.a'):
                shutil.move (os.path.join(root,file), path)

def configure_proc (*args):
    cmd = ['sh','-c','CFLAGS=-std=gnu89', file_configure]
    for a in args:
        cmd += a
    info (cmd)
    return subprocess.call (cmd)


def make_proc (*args):
    cmd = ['make','-C', path_current,'-j']
    for a in args:
        cmd += a
    info (cmd)
    return subprocess.call (cmd)

def job (path_target, *args):
    info ('Build dependency: {dep_name}'.format (dep_name='ucl'))
    if os.path.exists(file_configure):
        if (configure_proc (['libutf8proc.a']) == 0) and os.path.exists (file_make):
            if make_proc () == 0:
                move_proc (path_target)
        else:
            error ('Make dependency error')
    else:
        error ('Don\'t found Makefile')
