import os, sys, subprocess, shutil

from colorize import *

def bs_result (path, path_target):
    for root, dirs, files in os.walk (path):
        for file in files:
            if file.endswith('.so'):
                shutil.move (os.path.realpath(os.path.join(root,file)), os.path.realpath (path_target))

def bs_make (path, args):
    cmd = ['make', '-j', '-C', path] + args
    hint (cmd)
    return subprocess.call (cmd)
