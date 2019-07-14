project ('utf8proc', 'c',
         version: '2.3.1',
         license: 'MIT',
         default_options:[
         ],
         meson_version: '>=0.46')

cc = meson.get_compiler('c')
cc_name = cc.get_id()
os_name = target_machine.system()

cc_args         = []
cc_link_args    = []
cc_defs         = []

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
if cc_name == 'msvc'
    cc_defs += '/DUTF8PROC_EXPORTS'
else
    cc_defs += '-DUTF8PROC_EXPORTS'
endif

if not (cc_name == 'msvc')
    # Build on linux (or difffernt *nix) system
    cc_args = '''
        -std=c99
        -O2
        -Wall
        -pedantic
    '''.split()

    cc_link_args = '''
    '''.split()
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

foreach arg : cc_defs
    add_project_arguments(arg, language: 'c')
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
