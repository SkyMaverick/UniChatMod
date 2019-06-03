libucm_src = files('''
    api.c
    core.c
    cpentupd.c
    db.c
    evhook.c
    mainloop.c
    logger.c
    mqueue.c
    plugmgr.c
    ucm.c
    unicode.c
'''.split())

osal_src = []
foreach src : '''
                osal-dynlib.c
                osal-dynlib.h
                osal-fs.c
                osal-fs.h
                osal-intrnl.c
                osal-intrnl.h
                osal-memory.c
                osal-memory.h
                osal-system.c
                osal-system.h
                osal-threading.c
                osal-threading.h
                osal-timers.c
                osal-timers.h
              '''.split()
    osal_src += 'osal' / src
endforeach

libucm_src += osal_src

libucm_deps = [
    system_library_dynload,
    system_library_uv
]

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
libucm_deps += u8dep

# split project version for move to config.h
libucm_version = meson.project_version().split('.')
libucm_commit = ''
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
        libucm_commit = git_commit.stdout().strip()
    else
        message ('@0@: don\'t get git revision this project'.format(meson.project_name()))
    endif
endif

# get build time
if host_machine.system() == 'windows'
    libucm_time = run_command('cmd', '/c', 'echo %date%_%time%')
else
    libucm_time = run_command('date', '+%Y-%m-%d_%H:%M:%S')
endif

if libucm_time.returncode() != 0
    libucm_time = ''
else
    libucm_time = libucm_time.stdout().strip()
endif

# full version string
libucm_version_full = '@0@_@1@_@2@'.format (
                            meson.project_version(),
                            libucm_commit,
                            libucm_time)

libucm_conf = configuration_data()

libucm_conf.set_quoted ('appname'         , meson.project_name())
libucm_conf.set_quoted ('version'         , meson.project_version())
libucm_conf.set        ('version_major'   , libucm_version[0])
libucm_conf.set        ('version_minor'   , libucm_version[1])
libucm_conf.set        ('version_patch'   , libucm_version[2])
libucm_conf.set_quoted ('version_full'    , libucm_version_full)
libucm_conf.set_quoted ('path_modules'    , get_option ('path_modules_lookup'))
libucm_conf.set_quoted ('path_libs'       , get_option ('path_libs_lookup'))
libucm_conf.set_quoted ('path_docs'       , get_option ('path_docs_lookup'))
libucm_conf.set_quoted ('build_commit'    , libucm_commit)
libucm_conf.set_quoted ('build_time'      , libucm_time)
libucm_conf.set_quoted ('build_target'    , '@0@_@1@'.format(target_machine.cpu(), target_machine.system()))
libucm_conf.set_quoted ('build_cc'        , '@0@_@1@'.format(cc.get_id(), cc.version()))
libucm_conf.set_quoted ('build_opts'      , '') #TODO compiler options
libucm_conf.set_quoted ('build_flags'     , '') #TODO compiler flags
libucm_conf.set        ('mq_size'         , get_option('message_queue_size'))
libucm_conf.set        ('session_count'   , get_option('session_count'))
libucm_conf.set        ('string_capacity' , get_option('string_buffer_capacity'))
libucm_conf.set        ('limit_plugins'   , get_option('limit_plugins_count'))

configure_file (input: 'config.h.in',
                output: 'config.h',
                configuration: libucm_conf)

core_header_c = files('.' / 'include' / 'ucm.h')
core_config_c = files(meson.current_build_dir() / 'config.h')

shared_headers += [
    core_header_c,
    core_config_c,
]

libucm = shared_library (meson.project_name(),
                          libucm_src,
                          dependencies: libucm_deps,
                          include_directories: libucm_inc,
                          build_rpath: libs_rpath,
                          install_rpath: libs_rpath,
                          install: true,
                          install_dir: install_path_root)
