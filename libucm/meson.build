libucm_src = files('''
    api.c
    core.c
    cpentupd.c
    db.c
    errors.h
    evhook.c
    flags.c
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
        osal-intrnl.h
        osal-memory.c
        osal-memory.h
        osal-threading.c
        osal-threading.h
        osal-uuid.c
        osal-uuid.h
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
        cmake = import ('cmake')
        u8proc_lib = cmake.subproject('utf8proc',
                 cmake_options: [
                     '='.join (['-DBUILD_SHARED_LIBS','ON'])
                 ])
        u8dep = u8proc_lib.dependency('utf8proc')
    endif
else
        cmake = import ('cmake')
        u8proc_lib = cmake.subproject('utf8proc',
                 cmake_options: [
                     '='.join (['-DBUILD_SHARED_LIBS','ON'])
                 ])
        u8dep = u8proc_lib.dependency('utf8proc')
endif
libucm_deps += u8dep

# add utf8proc dependency
if not (ccid == 'msvc')
    if not get_option('custom_libs')
        uuid_dep = cc.find_library('uuid', required: false)
        if not uuid_dep.found()
            uuid_sp = subproject('uuid')
            uuid_dep = uuid_sp.get_variable('libuuid')
        endif
    else
            uuid_sp = subproject('uuid')
            uuid_dep = uuid_sp.get_variable('libuuid')
    endif
    libucm_deps += uuid_dep
endif


# split project version for move to config.h
libucm_version = meson.project_version().split('.')
libucm_commit = ''
# get git commit number
git_app = find_program('git', required: false)
if git_app.found()
    repo = meson.current_source_dir()
    if get_option('create_with_full_commit')
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
libucm_conf.set_quoted ('path_plugins'    , install_path_plugin)
libucm_conf.set_quoted ('path_libs'       , install_path_libs)
libucm_conf.set_quoted ('path_docs'       , '')
libucm_conf.set_quoted ('build_commit'    , libucm_commit)
libucm_conf.set_quoted ('build_time'      , libucm_time)
libucm_conf.set_quoted ('build_target'    , '@0@_@1@'.format(target_machine.cpu(), target_machine.system()))
libucm_conf.set_quoted ('build_cc'        , '@0@_@1@'.format(cc.get_id(), cc.version()))
libucm_conf.set_quoted ('build_opts'      , '')#TODO compiler options
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
libucm_part_headers = []

foreach item : '''
      ucontacts.h
      udatabase.h
      udefs.h
      uevents.h
      ugui.h
      umacro.h
      umain.h
      uplugin.h
      uprotocols.h
      usignals.h
    '''.split()
    libucm_part_headers += files('.'/'include'/'uni'/ item)
endforeach
install_headers (libucm_part_headers,
                 subdir: 'uni')

libucm = shared_library (meson.project_name(),
                          libucm_src,
                          dependencies: libucm_deps,
                          include_directories: libs_inc,
                          build_rpath: libs_rpath,
                          install_rpath: libs_rpath,
                          install: true)
libucma = static_library (meson.project_name(),
                          libucm_src,
                          dependencies: libucm_deps,
                          include_directories: libs_inc,
                          build_rpath: libs_rpath,
                          install: false)

