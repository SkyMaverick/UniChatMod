project ('ucm', 'c',
         version: '0.0.1a',
         license: 'Zlib',
         default_options:[
            'c_std=gnu11'
         ],
         meson_version: '>=0.40')

cc = meson.get_compiler('c')

if get_option('buildtype').startswith('debug')
    add_global_arguments ('-DDEBUG', language: 'c')
endif
if get_option('enable_traces')
    add_global_arguments ('-DENABLE_TRACE_MESSAGES', language: 'c')
endif

# split project version for move to config.h
lib_core_version = meson.project_version().split('.')

# get git commit number
git_app = find_program('git', required: false)
if git_app.found()
    repo = meson.current_source_dir()
    git_commit = run_command(git_app, '-C', repo, 'rev-parse','--short','HEAD')
    if git_commit.returncode() == 0
        lib_core_commit = git_commit.stdout().strip()
    else
        message ('@0@: don\'t get git revision this project'.format(meson.project_name()))
    endif
endif

# get build time
if host_machine.system() == 'windows'
    build_time = run_command('cmd', '/c', 'echo %date%_%time%')
else
    build_time = run_command('date', '+%Y-%m-%d_%H:%M:%S')
endif

if build_time.returncode() != 0
    build_time = ''
else
    build_time = build_time.stdout().strip()
endif

# full version string
lib_core_version_full = '@0@_@1@_@2@'.format (
                            meson.project_version(),
                            lib_core_commit,
                            build_time)

lib_core_conf = configuration_data()
lib_core_conf.set ('version', meson.project_version())
lib_core_conf.set ('version_major', lib_core_version[0])
lib_core_conf.set ('version_minor', lib_core_version[1])
lib_core_conf.set ('version_patch', lib_core_version[2])
lib_core_conf.set ('version_full',  lib_core_version_full)
lib_core_conf.set ('commit', lib_core_commit)
lib_core_conf.set ('build_time', build_time)
lib_core_conf.set ('mq_size', get_option('message_queue_size'))
lib_core_conf.set ('string_capacity', get_option('string_buffer_capacity'))

lib_core_inc = include_directories ('.',
                                    'lib_core'
               )

subdir('lib_core')

if get_option('build_with_plugins')
    subdir('plugins')
endif
