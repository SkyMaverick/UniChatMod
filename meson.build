project ('ucm', 'c',
         version: '0.0.1',
         license: 'Zlib',
         default_options:[
            'c_std=gnu11'
         ],
         meson_version: '>=0.44')

cc = meson.get_compiler('c')

if get_option('buildtype').startswith('debug')
    add_global_arguments ('-DDEBUG', language: 'c')
endif
if get_option('enable_traces')
    add_global_arguments ('-DENABLE_TRACE_MESSAGES', language: 'c')
endif

lib_core_inc = include_directories ('.',
                                    'lib_core'
               )

subdir('lib_core')

if get_option('build_with_plugins')
    subdir('plugins')
endif

if get_option('build_with_gui')
    subdir('gui')
endif
