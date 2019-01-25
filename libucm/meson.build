libcore_src = files('''
    api.c
    core.c
    cpentupd.c
    db.c
    evhook.c
    mainloop.c
    osal.c
    logger.c
    mqueue.c
    plugmgr.c
    ucm.c
    unicode.c
'''.split())

#libcore_osal_src = '''
#        osal.c
#'''.split()
#
#foreach item : libcore_osal_src
#    libcore_src += files ('..' / 'osal' / item)
#endforeach
#
libcore_deps = [
    system_library_threads,
    system_library_dynload
]

# add plibsys dependency
if not get_option('custom_libs')
    plsdep = cc.find_library('plibsys', required: false)
    if not plsdep.found()
        pls_lib = subproject('plibsys', default_options: ['='.join (['path_install', install_path_libs])])
        plsdep = pls_lib.get_variable('plibsys_dep')
    endif
else
    pls_lib = subproject('plibsys', default_options: ['='.join (['path_install', install_path_libs])])
    plsdep = pls_lib.get_variable('plibsys_dep')
endif
libcore_deps += plsdep

# add utf8proc dependency
if not get_option('custom_libs')
    u8dep = cc.find_library('utf8proc', required: false)
    if not u8dep.found()
        u8proc_lib = subproject('utf8proc', default_options: ['='.join (['path_install', install_path_libs])])
        u8dep = u8proc_lib.get_variable('u8proc_dep')
    endif
else
    u8proc_lib = subproject('utf8proc', default_options: ['='.join (['path_install', install_path_libs])])
    u8dep = u8proc_lib.get_variable('u8proc_dep')
endif
libcore_deps += u8dep

# add libuv dependency
if not get_option('custom_libs')
    uvdep = cc.find_library('uv', required: false)
    if not u8dep.found()
        uv_lib = subproject('uv', default_options: ['='.join (['path_install', install_path_libs])])
        u8dep = uv_lib.get_variable('uv_dep')
    endif
else
    uv_lib = subproject('uv', default_options: ['='.join (['path_install', install_path_libs])])
    uvdep = uv_lib.get_variable('uv_dep')
endif
libcore_deps += uvdep

# split project version for move to config.h
lib_core_version = meson.project_version().split('.')
lib_core_commit = ''
# get git commit number
git_app = find_program('git', required: false)
if git_app.found()
    repo = meson.current_source_dir()
    if get_option('build_with_full_commit')
        git_commit = run_command(git_app, '-C', repo, 'rev-parse','HEAD')
    else
        git_commit = run_command(git_app, '-C', repo, 'rev-parse','--short','HEAD')
    endif
    if git_commit.returncode() == 0
        lib_core_commit = git_commit.stdout().strip()
    else
        message ('@0@: don\'t get git revision this project'.format(meson.project_name()))
    endif
endif

# get build time
if host_machine.system() == 'windows'
    lib_core_time = run_command('cmd', '/c', 'echo %date%_%time%')
else
    lib_core_time = run_command('date', '+%Y-%m-%d_%H:%M:%S')
endif

if lib_core_time.returncode() != 0
    lib_core_time = ''
else
    lib_core_time = lib_core_time.stdout().strip()
endif

# full version string
lib_core_version_full = '@0@_@1@_@2@'.format (
                            meson.project_version(),
                            lib_core_commit,
                            lib_core_time)

lib_core_conf = configuration_data()

lib_core_conf.set_quoted ('appname'         , meson.project_name())
lib_core_conf.set_quoted ('version'         , meson.project_version())
lib_core_conf.set        ('version_major'   , lib_core_version[0])
lib_core_conf.set        ('version_minor'   , lib_core_version[1])
lib_core_conf.set        ('version_patch'   , lib_core_version[2])
lib_core_conf.set_quoted ('version_full'    , lib_core_version_full)
lib_core_conf.set_quoted ('path_modules'    , get_option ('path_modules_lookup'))
lib_core_conf.set_quoted ('path_docs'       , get_option ('path_docs_lookup'))
lib_core_conf.set_quoted ('build_commit'    , lib_core_commit)
lib_core_conf.set_quoted ('build_time'      , lib_core_time)
lib_core_conf.set_quoted ('build_target'    , '@0@_@1@'.format(target_machine.cpu(), target_machine.system()))
lib_core_conf.set_quoted ('build_cc'        , '@0@_@1@'.format(cc.get_id(), cc.version()))
lib_core_conf.set_quoted ('build_opts'      , '') #TODO compiler options
lib_core_conf.set_quoted ('build_flags'     , '') #TODO compiler flags
lib_core_conf.set        ('mq_size'         , get_option('message_queue_size'))
lib_core_conf.set        ('session_count'   , get_option('session_count'))
lib_core_conf.set        ('string_capacity' , get_option('string_buffer_capacity'))
lib_core_conf.set        ('limit_plugins'   , get_option('limit_plugins_count'))

configure_file (input: 'config.h.in',
                output: 'config.h',
                configuration: lib_core_conf)

core_header_c = files('.' / 'include' / 'ucm.h')
core_header_pas = files('.' / 'bindings' / 'fpc' / 'ucm.pas')

install_headers (core_header_c,
                 core_header_pas,
                 meson.current_build_dir() / 'config.h',
                 install_dir: install_path_devs)

libcore = shared_library (meson.project_name(),
                          libcore_src,
                          dependencies: libcore_deps,
                          include_directories: lib_core_inc,
                          build_rpath: libs_rpath,
                          install_rpath: libs_rpath,
                          install: true,
                          install_dir: install_path_root)

#libcore_a = static_library (meson.project_name(),
#                            libcore_src,
#                            dependencies: libcore_deps,
#                            include_directories: lib_core_inc,
#                            install: false,
#                            install_dir: install_path_root)
