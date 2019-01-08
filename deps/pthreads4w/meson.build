project ('pthread', 'c',
         version: '3.0.0',
         license: 'Apache license',
         default_options:[],
         meson_version: '>=0.49')

cc = meson.get_compiler('c')

if not (cc.get_id() == 'msvc')
    error ('This library only for Microsoft Windows build with Visual Studio')
endif

ptwin_src = files ('''
    pthread.c
    config.h
'''.split())

ptwin_header = files ('pthread.h')

cc_args = '''
'''.split()

cc_link_args = '''
'''.split()

foreach arg : cc_args
    if cc.has_argument(arg)
        add_project_arguments(arg, language: 'c')
    endif
endforeach

foreach arg : cc_link_args
    if cc.has_link_argument (arg)
        add_project_link_arguments(arg, language: 'c')
    endif
endforeach

ptwin_deps = []
ptwin_inc = include_directories('.')

add_project_arguments ('/DHAVE_CONFIG_H', language: 'c')
add_project_arguments ('/D__PTW32_BUILD_INLINED', language: 'c')
add_project_arguments ('/D__PTW32_CLEANUP_C', language: 'c')

# ==================================================
#   INCLUDES AND TARGETS
# ==================================================

if meson.is_subproject()
    message ('install dir: @0@'.format (get_option('path_install')))

    ptwin_lib = shared_library ( meson.project_name(),
                                 ptwin_src,
                                 dependencies: ptwin_deps,
                                 install: true,
                                 install_dir: get_option('path_install'),
                              )

    ptwin_dep = declare_dependency (
                include_directories: ptwin_inc,
                link_with: ptwin_lib
            )    
else
    ptwin_lib = shared_library ( meson.project_name(),
                                ptwin_src,
                                dependencies: ptwin_deps,
                                install: true,
                              )

endif
