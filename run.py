#!/usr/bin/env python3

import os, subprocess, shutil, fnmatch, platform

from sys import argv
from colorize import *

project_name = 'ucm'
path_script = os.path.abspath (os.path.curdir)
path_build  = os.path.join (path_script, 'build', os.name)
path_libs   = os.path.join (path_build, 'libs')

path_bundle = os.path.join (path_script, 'bundle')
path_packages = os.path.join (path_script, 'pkgs')

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

def build_cmd (type, prefix):
    if os.path.exists(path_build):
        meson_cmd ('--reconfigure')
    else:
        meson_cmd ('--buildtype='+type, '-Dprefix='+prefix)

def shell_cmd_out (app, *args):
    cmd = [app]
    for i in args:
       cmd += [i]
    
    shell=subprocess.Popen (cmd, stdout=subprocess.PIPE)
    data=shell.communicate()
    return (data)[0].decode()

package_name = project_name +'-' \
                + (shell_cmd_out('git', 'describe', '--abbrev=0', '--tags').rstrip()) + '-' \
                + platform.system().lower() + '_'\
                + platform.architecture()[0].lower()

# ==================================================
#   ACTIONS
# ==================================================

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
          
def action_build ():
    return ninja_cmd()

def action_debug ():
    build_cmd ('debug', path_bundle)
    return action_build()

def action_release ():
    build_cmd ('release', path_bundle)
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

def action_dockerhub ():
    info ('Update docker image on DockerHub (signin if need)')
    action_clean ()
    result = 1
    if os.path.exists (file_shell_travis):
        result = shell_cmd(file_shell_travis, 'UPDATE_DH')
    else:
        error ('Don\'t found travis shell file: {file}'.format(file=file_shell_travis))
    return result

def action_bundle ():
    info ('Create application bundle in: {path}'.format(path=path_bundle))
    action_clean()
    action_debug()
    ninja_cmd('install')

def action_arcxz ():
    tmp_path = os.path.join (path_packages, 'temp')
    build_cmd ('release', tmp_path)
    ninja_cmd ('install')
    if platform.system().lower() == 'windows':
        shell_cmd ('7z', 'a', '-tzip', '-mx9',
                    os.path.join (path_packages, package_name+'.zip'),
                    os.path.join (tmp_path, project_name), '.')
    else:
        shell_cmd ('tar', 'cvfJ',
                    os.path.join (path_packages, package_name+'.tar.xz'),
                    '-C', os.path.join (tmp_path, project_name), '.')
    remove_dir (tmp_path)

def action_dummy ():
    info ("Run dummy function for test")
    return

# ==================================================
#   ACTIONS BINDINGS
# ==================================================

actions = {
        'build'             : action_build,
        'debug'             : action_debug,
        'release'           : action_release,
        'clean'             : action_clean,
        'test'              : action_test,
        'log'               : action_log,
        'pkg_src'           : action_dummy,
        'laz_gui'           : action_dummy,
        'docker_hub'        : action_dockerhub,
        'bundle'            : action_bundle,
        'pack_arc'          : action_arcxz
}

# ==================================================
#   ACTIONS CHANGER
# ==================================================


try:
    action = actions [argv[1]]
except KeyError as e:
    print ("I don't know this command: {}".format(argv[1]))
    exit (1)
else:
    exit (action())
