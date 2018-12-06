#!/usr/bin/env python3

import os, sys, subprocess, shutil, fnmatch
from colorize import *

path_script = os.path.abspath (os.path.curdir)
path_build  = os.path.join (path_script, 'build')
path_libs  = os.path.join (path_build, 'libs')

file_shell_travis = os.path.join(path_script, 'tools', 'travis', 'manager.sh')

_clean_files = '''
    *.so
    *.a
    *.bak
    *.log
    *.la
    *.lo
    *.dbg
'''.split()

_clean_paths = '''
    build
    pkgs
    bundle
'''.split()

_ignore_paths = [
    'deps'
]

# =================================================
# SERVICE FUNCTIONS 
# ==================================================

def ninja_cmd (*args):
    result = 1
    if os.path.exists ( os.path.join (path_build, 'build.ninja')):
        os.chdir (path_build)

        cmd = ['ninja']
        for i in args:
            cmd += [i]
        result = subprocess.call(' '.join(cmd), shell=True)

        os.chdir (path_script)
    else:
        error ('Meson configure files don\'t create')
    return result

def meson_cmd (*args):
    cmd = ['meson'] + ['.'] + [path_build]
    for i in args:
        cmd += [i]
    return subprocess.call(' '.join(cmd), shell=True)

def shell_cmd (shell, *args):
    cmd = [shell]
    for i in args:
       cmd += [i]
    
    return subprocess.call (' '.join(cmd), env=os.environ.copy(), shell=True)


def remove_dir (path):
    if os.path.exists (path):
        shutil.rmtree (path, ignore_errors=False, onerror=None)

# ==================================================
#   ACTIONS
# ==================================================

def action_debug ():
    meson_cmd()
    return ninja_cmd()

def action_release ():
    meson_cmd('--buildtype=release')
    return ninja_cmd()

def action_clean():
    info ('Cleanup in source dir: {path}'.format(path=path_script))
    remove_dir (path_build)
    for path_base, dirs, files in os.walk (path_script):
        for item in dirs:
            if item in _ignore_paths:
                dirs.remove (item)
            if item in _clean_paths:
                remove_dir (item)
        for fname in files:
            for i in _clean_files:
                strName = os.path.join(path_base, fname)
                if fnmatch.fnmatch(os.path.basename(strName), i): 
                    if os.access(strName, os.W_OK):
                        os.remove(strName)
          

def action_new():
    action_clean()
    return action_build()

def action_test():
    info ('Start test framework in: {path}'.format(path=path_build))
    return ninja_cmd('test_bot')

def action_log():
    info ('Meson system build log in: {path}'.format(path=path_build))
    flog_path = os.path.join(path_build, 'meson-logs','meson-log.txt')
    if os.path.exists (flog_path) :
        flog_file = open(flog_path, 'r')
        print (flog_file.read())
        flog_file.close()
    else:
        error ('Meson configure files don\'t create')

def action_install():
    info ('Install application into: {path}'.format(path=path_build))
    return ninja_cmd('install')
    
def action_uninstall():
    info ('Uninstall application into: {path}'.format(path=path_build))
    return ninja_cmd('uninstall')

def action_dockerhub ():
    info ('Update docker image on DockerHub (signin if need)')
    action_clean ()
    result = 1
    if os.path.exists (file_shell_travis):
        result = shell_cmd(file_shell_travis, 'UPDATE_DH')
    else:
        error ('Don\'t found travis shell file: {file}'.format(file=file_shell_travis))
    return result

def action_dummy ():
    info ("Run dummy function for test")
    return

# ==================================================
#   ACTIONS BINDINGS
# ==================================================

actions = {
        'debug'             : action_debug,
        'release'           : action_release,
        'clean'             : action_clean,
        'test'              : action_test,
        'log'               : action_log,
        'pkg_src'           : action_dummy,
        'laz_gui'           : action_dummy,
        'install'           : action_install,
        'uninstall'         : action_uninstall,
        'docker_hub'        : action_dockerhub,
}

# ==================================================
#   ACTIONS CHANGER
# ==================================================


try:
    action = actions [sys.argv[1]]
except KeyError as e:
    print ("I don't know this command: {}".format(sys.argv[1]))
    exit (1)
else:
    exit (action())
