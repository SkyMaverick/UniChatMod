project ('ucm', 'c',
         version: '0.0.1',
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

lib_core_conf = configuration_data()
lib_core_conf.set ('version', meson.project_version())
lib_core_conf.set ('mq_size', get_option('message_queue_size'))
lib_core_conf.set ('string_capacity', get_option('string_buffer_capacity'))

lib_core_inc = include_directories ('.',
                                    'lib_core'
               )

subdir('lib_core')

if get_option('build_with_plugins')
    subdir('plugins')
endif
