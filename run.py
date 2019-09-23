#!/usr/bin/env python3

import os, subprocess, shutil, fnmatch, platform

from sys import argv
from colorize import *

project_name = 'ucm'
project_version = '0.1.3'


path_build_pathname = 'build'

_clean_files = '''
    *.so
    *.a
    *.bak
    *.log
    *.la
    *.lo
    *.dbg
'''.split()

ignore_format_paths = [
    '.git',
    'deps',
    'tools',
    'build'
]

format_exts = '''
    *.c
    *.cpp
    *.cxx
    *.h
    *.hpp
    *.hxx
'''.split()

# =================================================
# SERVICE FUNCTIONS
# ==================================================
def meson_internal (path, *args):
    cmd = ['meson'] + ['.'] + [path]
    for i in args:
        cmd += [i]

    print (' '.join(cmd))
    return subprocess.call(' '.join(cmd), shell=True)

def ninja_cmd (path, *args):
    result = 1
    if os.path.exists ( os.path.join (path, 'build.ninja')):
        curr_path = os.path.curdir
        os.chdir (path)

        cmd = ['ninja']
        for i in args:
            cmd += [i]
        result = subprocess.call(' '.join(cmd), shell=True)

        os.chdir (curr_path)
    else:
        error ('Meson configure files don\'t create')
    return result

def shell_cmd (shell, *args):
    cmd = [shell]
    for i in args:
       cmd += [i]

    return subprocess.call (' '.join(cmd), env=os.environ.copy(), shell=True)

def shell_cmd_out (app, *args):
    cmd = [app]
    for i in args:
       cmd += [i]

    shell=subprocess.Popen (cmd, stdout=subprocess.PIPE)
    data=shell.communicate()
    return (data)[0].decode()

def meson_cmd (type, cross, path, prefix):
    args = ['--buildtype='+type, '-Dprefix='+prefix]
    if os.path.exists(path):
        args += ['--reconfigure']
    if cross:
        args += ['--cross-file', cross]
    if (type == 'release') \
        and (platform.system().lower() != 'windows'):
        args += ['--strip']
    return meson_internal (path, *args)

def remove_dir (path):
    if os.path.exists (path):
        return shutil.rmtree (path, ignore_errors=False, onerror=None)

def open_all (path):
    for root, dirs, files in os.walk (path):
        for d in dirs:
            os.chmod (os.path.join(root,d), 0o777)

def move_with_replace (file, path):
    try:
        fname = os.path.basename(file)
        if os.path.exists (os.path.join (path, fname)):
            os.remove (os.path.join(path, fname))
    except OSError:
        pass
    else:
        shutil.move (file, path)

def copytree2 (src, dst):
    if not os.path.exists (dst):
        os.makedirs (dst)
    for item in os.listdir(src):
        s = os.path.join(src, item)
        d = os.path.join(dst, item)
        if os.path.isdir(s):
            shutil.copytree(s, d, False, None)
        else:
            shutil.copy(s, d)

package_name = project_name +'-' \
                + platform.system().lower() + '_'\
                + platform.architecture()[0].lower()

# ==================================================
#   ACTIONS
# ==================================================

def action_clean(**defs):
    path_script = defs['path_script']
    path_build = defs['path_build']

    info ('Cleanup in source dir: {path}'.format(path=path_script))
    remove_dir (path_build)
    for path_base, dirs, files in os.walk (path_script):
        for fname in files:
            for i in _clean_files:
                strName = os.path.join(path_base, fname)
                if fnmatch.fnmatch(os.path.basename(strName), i):
                    if os.access(strName, os.W_OK):
                        os.remove(strName)

def action_clean_all(**defs):
    info ('Cleanup in source dir: {path}'.format(path=defs['path_script']))
    remove_dir (defs['path_make'])

def action_ninja (**defs):
    return ninja_cmd(defs['path_build'])

def action_debug (**defs):
    if 'cross_file' in defs:
        cfile = defs['cross_file']
    else:
        cfile = ''

    meson_cmd ('debug', cfile, defs ['path_build'], defs ['path_bundle'])
    return action_ninja (**defs)

def action_release (**defs):
    if 'cross_file' in defs:
        cfile = defs['cross_file']
    else:
        cfile = ''

    meson_cmd ('release', cfile, defs ['path_build'], defs ['path_bundle'])
    return action_ninja (**defs)

def action_test(**defs):
    info ('Start test framework in: {path}'.format(path=path_build))
    return ninja_cmd(defs['path_build'], 'test_bot')

def action_log(**defs):
    path_build = defs['path_build']

    info ('Meson system build log in: {path}'.format(path=path_build))
    flog_path = os.path.join(path_build, 'meson-logs','meson-log.txt')
    if os.path.exists (flog_path) :
        flog_file = open(flog_path, 'r')
        print (flog_file.read())
        flog_file.close()
    else:
        error ('Meson configure files don\'t create')

def action_bundle (**defs):
    path_bundle = defs['path_bundle']
    path_build  = defs['path_build']

    info ('Create application bundle in: {path}'.format(path=path_bundle))
    if platform.system().lower() == 'windows':
        if ninja_cmd(path_build, 'install') == 0:
            for root, dirs, files in os.walk (path_bundle):
               for item in files:
                   if item.endswith('.lib') or \
                      item.endswith('.pdb'):
                       os.remove ( os.path.join(root, item) )
            return 0
        else:
            return 1
    else:
        return ninja_cmd(path_build, 'install')

def action_bundle_dbg (**defs):
    path_bundle = defs['path_bundle']
    path_build  = defs['path_build']

    info ('Create application DEBUG bundle in: {path}'.format(path=path_bundle))
    return ninja_cmd(path_build, 'install')

def action_arcxz (**defs):
    if (action_bundle (**defs) != 0):
        return 1
    path_bundle     = defs ['path_bundle'   ]
    path_packages   = defs ['path_packages' ]
    path_script     = defs ['path_script'   ]

    open_all (path_build_root)
    if os.path.exists(path_bundle):
        if not os.path.exists(path_packages):
            os.makedirs(path_packages)
        if platform.system().lower() == 'windows':
            return shell_cmd ('7z', 'a', '-tzip', '-mx9',
                        os.path.join (path_packages, package_name+'.zip'),
                        path_bundle, '.')
        else:
            return shell_cmd ('tar', 'cvfJ',
                        os.path.join (path_packages, package_name+'.tar.xz'),
                        '-C', path_bundle, '.')

def action_deb (**defs):
    if (action_bundle (**defs) != 0):
        return 1
    path_bundle     = defs ['path_bundle'   ]
    path_packages   = defs ['path_packages' ]
    path_script     = defs ['path_script'   ]
    path_temp       = defs ['path_temp'     ]

    open_all (path_build_root)
    path_debconf = os.path.join (path_script, 'tools', 'packages', 'debian')
    if os.path.exists (path_bundle):
        if platform.system().lower() == 'linux':
            if not os.path.exists (path_packages):
                os.makedirs (path_packages)
            path_tmpdeb = os.path.join (path_temp, 'deb')
            remove_dir (path_tmpdeb)

            copytree2 (path_debconf, path_tmpdeb)
            copytree2 (path_bundle, os.path.join(path_tmpdeb, 'opt'))

            os.chdir (path_tmpdeb)
            if (shell_cmd (os.path.join(path_tmpdeb,'build.sh'), \
                       project_name, \
                       project_version) == 0):

                for paths, dirs, files in os.walk (path_tmpdeb):
                    for item in files:
                        if item.endswith('.deb'):
                            move_with_replace (os.path.join(paths,item), path_packages)
                os.chdir (path_script)
                return 0
            else:
                os.chdir (path_script)
                return 1

def action_shell (**defs):
    if (action_bundle (**defs) != 0):
        return 1
    path_bundle     = defs ['path_bundle'   ]
    path_packages   = defs ['path_packages' ]
    path_script     = defs ['path_script'   ]
    path_temp       = defs ['path_temp'     ]

    open_all (path_build_root)
    path_shconf = os.path.join (path_script, 'tools', 'packages', 'shell')
    if os.path.exists (path_bundle):
        if platform.system().lower() == 'linux':
            if not os.path.exists (path_packages):
                os.makedirs (path_packages)
            path_tmpsh = os.path.join (path_temp, 'shell')
            remove_dir (path_tmpsh)
            copytree2 (path_shconf, path_tmpsh)
            copytree2 (path_bundle, os.path.join(path_tmpsh, project_name))
            os.chdir (path_tmpsh)
            if (shell_cmd (os.path.join(path_tmpsh,'build.sh'),     \
                        path_packages,                              \
                        project_name,                               \
                        project_version) == 0):
                os.chdir (path_script)
                return 0
            else:
                os.chdir (path_script)
                return 1

def action_7z (**defs):
    if (action_bundle (**defs) != 0):
        return 1
    path_bundle     = defs ['path_bundle'   ]
    path_packages   = defs ['path_packages' ]
    path_script     = defs ['path_script'   ]
    path_temp       = defs ['path_temp'     ]

    open_all (path_build_root)
    path_bat = os.path.join (path_script, 'tools', 'packages', '7z')
    if os.path.exists (path_bundle):
        if platform.system().lower() == 'windows':
            if not os.path.exists (path_packages):
                os.makedirs (path_packages)
            path_tmpsfx = os.path.join (path_temp, '7z')
            remove_dir (path_tmpsfx)

            print (path_bundle)
            print (path_tmpsfx)
            copytree2 (path_bat, path_tmpsfx)
            copytree2 (path_bundle, os.path.join(path_tmpsfx, project_name))
            os.chdir (path_tmpsfx)
            if (shell_cmd (os.path.join(path_tmpsfx,'build.bat'),   \
                        path_packages,                              \
                        project_name,                               \
                        project_version) == 0):
                os.chdir (path_script)
                return 0
            else:
                os.chdir (path_script)
                return 1

def action_format (**defs):
    path = defs ['path_script']
    for root, dirs, files in os.walk (path):
        for item in dirs:
            if item in ignore_format_paths:
                dirs.remove(item)
        for item in files:
            absName = os.path.join(root, item)
            for ext in format_exts:
                if fnmatch.fnmatch(os.path.basename(absName), ext):
                    print ('Format file > {0}'.format(os.path.join(root,item)))
                    if shell_cmd ('clang-format', '-i', absName) != 0:
                        error ('Error formating file: {0}'.format(item))

def action_dummy (**defs):
    info ("Run dummy function for test")
    return

# ==================================================
#   ACTIONS BINDINGS
# ==================================================

actions = {
        'build'             : action_ninja,
        'debug'             : action_debug,
        'release'           : action_release,
        'format'            : action_format,
        'clean'             : action_clean,
        'clean_all'         : action_clean_all,
        'test'              : action_test,
        'log'               : action_log,
        'pkg_src'           : action_dummy,
        'bundle'            : action_bundle,
        'bundle_dbg'        : action_bundle_dbg,
        'pack_arc'          : action_arcxz,
        'pack_deb'          : action_deb,
        'pack_sh'           : action_shell,
        'pack_7z'           : action_7z
}

# ==================================================
#   ACTIONS CHANGER
# ==================================================


try:
    action = actions [argv[1]]

    path_current = os.path.abspath (os.path.curdir)
    path_build_root = os.path.join (path_current, path_build_pathname)

    if ( len(argv) > 2):
        args = {
            'target'        : argv[2],
            'path_build'    : os.path.join (path_build_root, argv[2]),
            'path_bundle'   : os.path.join (path_build_root, argv[2], 'bundle', project_name),

            'path_script'   : path_current,
            'path_make'     : path_build_root,
            'path_packages' : os.path.join (path_build_root, 'pkgs'),
            'path_temp'     : os.path.join (path_build_root, 'temp'),

            'cross_file'    : os.path.join (path_current, ''.join([argv[2],'_cross','.txt']))
        }
    else:
        args = {
            'target'        : os.name,
            'path_build'    : os.path.join (path_build_root, os.name),
            'path_bundle'   : os.path.join (path_build_root, os.name, 'bundle', project_name),

            'path_script'   : path_current,
            'path_make'     : path_build_root,
            'path_packages' : os.path.join (path_build_root, 'pkgs'),
            'path_temp'     : os.path.join (path_build_root, 'temp'),
        }
except KeyError as e:
    error ("I don't know this command: {}".format(argv[1]))
    exit (1)
else:
    exit (action(**args))
