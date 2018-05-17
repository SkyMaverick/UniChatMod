configure_file (input: 'config.h.in',
                output: 'config.h',
                configuration: lib_core_conf)

libcore_src = files(''' 
  api.c
  api.h
  core.c
  core.h
  defs.h
  mainloop.c
  mainloop.h
  gettext.h
  logger.c
  logger.h
  mqueue.c
  mqueue.h
  plugmgr.c
  plugmgr.h
  threading.c
  threading.h
  ucm.h
'''.split())

libcore_args = [
    '-Wall',
    '-Winline'
]

libcore_deps = [
    cc.find_library('pthread', required: false),
    cc.find_library('dl', required: false)
]

foreach arg : libcore_args
    if cc.has_argument(arg)
        add_project_arguments(arg, language : 'c')
    endif
endforeach

if get_option('buildtype').startswith('debug')
    foreach arg : [
            '-O0',
            '-g'
        ]
        if cc.has_argument(arg)
            add_project_arguments(arg, language : 'c')
        endif
    endforeach
elif get_option('buildtype').startswith('release')
    foreach arg : [
            '-O3',
            '-s'
        ]
        if cc.has_argument(arg)
            add_project_arguments(arg, language : 'c')
        endif
    endforeach
endif

libcore = shared_library (meson.project_name(), 
                          libcore_src,
                          dependencies: libcore_deps,
                          include_directories: lib_core_inc)

libcore_a = static_library (meson.project_name(), 
                            libcore_src,
                            dependencies: libcore_deps,
                            include_directories: lib_core_inc)
