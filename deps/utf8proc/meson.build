project ('utf8proc', 'c',
         version: '2.2.0',
         license: 'MIT',
         default_options:[
            'c_std=c99'
         ],
         meson_version: '>=0.46')

cc = meson.get_compiler('c')

# ==================================================
#   SOURCE FILES
# ==================================================
u8proc_header = files ('utf8proc.h')

u8proc_src = '''
    utf8proc.c
    utf8proc.h
'''.split()

# ==================================================
#   BUILD ARGUMENTS
# ==================================================
add_project_arguments ('-DUTF8PROC_EXPORTS', language: 'c')

cc_name = cc.get_id()
if (cc_name == 'gcc') or (cc_name == 'clang')
    # Build on linux (or difffernt *nix) system
    cc_args = '''
        -O2
        -Wall
        -pedantic
    '''.split()

    cc_link_args = '''
    '''.split()
elif cc_name == 'msvc'
    # Build on Windows with native Visual Studio
    cc_args = '''
    '''.split()

    cc_link_args = '''
    '''.split()
else
    # Build with default params for unknown compilator
    cc_args = []
    cc_link_args = []
endif

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

u8proc_deps = []

# ==================================================
#   INCLUDES AND TARGETS
# ==================================================

u8proc_inc = include_directories('.')

if meson.is_subproject()
    message ('install dir: @0@'.format (get_option('path_install')))

    u8proc_lib = shared_library ( meson.project_name(),
                                u8proc_src,
                                dependencies: u8proc_deps,
                                install: true,
                                install_dir: get_option('path_install')
                              )

    u8proc_dep = declare_dependency (
                include_directories: u8proc_inc,
                link_with: u8proc_lib
            )    
else
    u8proc_lib = shared_library ( meson.project_name(),
                                u8proc_src,
                                dependencies: u8proc_deps,
                                install: true
                              )

endif
