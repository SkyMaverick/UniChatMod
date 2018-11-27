#!/usr/bin/env python3

import os, sys, subprocess, shutil, fnmatch

from colorize import info, error
from deps.builder import build_dep as dependency

path_script = os.path.abspath (os.path.curdir)
path_build  = os.path.join (path_script, 'build')
file_shell_travis = os.path.join(path_script, 'tools', 'travis', 'manager.sh')

_clean = '''
    *.so
    *.a
    *.bak
    *.log
    *.la
    *.lo
    *.dbg
'''.split()

# ==================================================
# SERVICE FUNCTIONS 
# ==================================================

def ninja_cmd (*args):
    if os.path.exists ( os.path.join (path_build, 'build.ninja')):
        os.chdir (path_build)

        cmd = ['ninja']
        for i in args:
            cmd += [i]
        subprocess.call(cmd)

        os.chdir (path_script)
    else:
        error ('Meson configure files don\'t create')

def meson_cmd (*args):
    cmd = ['meson'] + ['.'] + [path_build]
    for i in args:
        cmd += [i]
    subprocess.call(cmd)

def shell_cmd (shell, *args):
    if sys.platform.lower() == 'windows':
        cmd = ['cmd', shell]
    else:
        cmd = ['sh', '-x', shell]

    for i in args:
       cmd += [i]

    subprocess.call (cmd)


def remove_dir (path):
    if os.path.exists (path):
        shutil.rmtree (path, ignore_errors=False, onerror=None)

# ==================================================
#   ACTIONS
# ==================================================

def action_build ():
    meson_cmd()
    ninja_cmd()

def action_build_dep():
    if not os.path.exists (path_build):
        os.mkdir(path_build)
    dependency ('utf8proc', path_build, '')
    dependency ('ucl', path_build, ''' 
                --enable-static
                --with-gnu-ld
                --with-pic
            '''.split())

def action_clean():
    info ('Cleanup in source dir: {path}'.format(path=path_script))
    remove_dir (path_build)
    for path_base, dirs, files in os.walk (path_script):
        for fname in files:
            for i in _clean:
                strName = os.path.join(path_base, fname)
                if fnmatch.fnmatch(os.path.basename(strName), i): 
                    if os.access(strName, os.W_OK):
                        os.remove(strName)
          

def action_new():
    action_clean()
    action_build()

def action_test():
    info ('Start test framework in: {path}'.format(path=path_build))
    ninja_cmd('test_bot')

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
    ninja_cmd('install')
    
def action_uninstall():
    info ('Uninstall application into: {path}'.format(path=path_build))
    ninja_cmd('uninstall')

def action_travis_daily ():
    info ('Start travis-ci daily build in: {path}'.format(path=path_build))
    action_clean ()
    if os.path.exists (file_shell_travis):
        if ( shell_cmd(file_shell_travis, 'CREATE_FAST') == 0 ):
            shell_cmd(file_shell_travis, 'RUN_DEBUG')
        shell_cmd(file_shell_travis, 'RUN_CLEANUP')
    else:
        error ('Don\'t found travis shell file: {file}'.format(file=file_shell_travis))

def action_travis_release ():
    info ('Start travis-ci release build in: {path}'.format(path=path_build)) 
    action_clean ()
    if os.path.exists (file_shell_travis):
        if ( shell_cmd(file_shell_travis, 'CREATE') == 0 ):
            shell_cmd(file_shell_travis, 'RUN_RELEASE')
        shell_cmd(file_shell_travis, 'RUN_CLEANUP')
    else:
        error ('Don\'t found travis shell file: {file}'.format(file=file_shell_travis))

def action_travis_coverity ():
    info ('Start travis-ci coverity check build in: {path}'.format(path=path_build)) 
    action_clean ()
    if os.path.exists (file_shell_travis):
        if ( shell_cmd(file_shell_travis, 'CREATE') == 0 ):
            shell_cmd(file_shell_travis, 'RUN_COVERITY')
        shell_cmd(file_shell_travis, 'RUN_CLEANUP')
    else:
        error ('Don\'t found travis shell file: {file}'.format(file=file_shell_travis))
 
def action_dockerhub ():
    info ('Update docker image on DockerHub (signin if need)')
    action_clean ()
    if os.path.exists (file_shell_travis):
        shell_cmd(file_shell_travis, 'UPDATE_DH')
    else:
        error ('Don\'t found travis shell file: {file}'.format(file=file_shell_travis))

def action_dummy ():
    info ("Run dummy function for test")
    return

# ==================================================
#   ACTIONS BINDINGS
# ==================================================

actions = {
        'build'             : action_build,
        'build_dep'         : action_build_dep,
        'new'               : action_new,
        'clean'             : action_clean,
        'test'              : action_test,
        'log'               : action_log,
        'pkg_src'           : action_dummy,
        'laz_gui'           : action_dummy,
        'install'           : action_install,
        'uninstall'         : action_uninstall,
        'travis_daily'      : action_travis_daily,
        'travis_release'    : action_travis_release,
        'travis_coverity'   : action_travis_coverity,
        'docker_hub'        : action_dockerhub,
}

# ==================================================
#   ACTIONS CHANGER
# ==================================================


try:
    action = actions [sys.argv[1]]
except KeyError as e:
    print ("I don't know this command: {}".format(sys.argv[1]))
else:
    action()
