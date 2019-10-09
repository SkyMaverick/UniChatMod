project ('ucl', 'c',
         version: '2.2.0',
         license: 'MIT',
         default_options:[
            'c_std=gnu89',
            'warning_level=2',
            'b_lto=true',
         ],
         meson_version: '>=0.46')

cc = meson.get_compiler('c')

# ==================================================
#   SOURCE FILES
# ==================================================
ucl_header = files ('ucl'/'ucl.h')

ucl_src = '''
    alloc.c
    n2b_99.c
    n2b_d.c
    n2b_ds.c    
    n2b_to.c
    n2d_99.c
    n2d_d.c
    n2d_ds.c
    n2d_to.c
    n2e_99.c
    n2e_d.c
    n2e_ds.c
    n2e_to.c
    ucl_crc.c
    ucl_init.c
    ucl_ptr.c
    ucl_str.c
    ucl_util.c
'''.split()

# ==================================================
#   BUILD ARGUMENTS
# ==================================================
cc_name = cc.get_id()
if (cc_name == 'gcc') or (cc_name == 'clang')
    # Build on linux (or difffernt *nix) system
    cc_args = '''
        -Wno-unused-local-typedefs
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

# TODO Add config
# add_project_arguments ('-DUCL_HAVE_CONFIG_H', language: 'c')
ucl_deps = []

# ==================================================
#   INCLUDES AND TARGETS
# ==================================================

ucl_inc = include_directories('.')

if meson.is_subproject()
    message ('install dir: @0@'.format (get_option('path_install')))

    ucl_lib = shared_library ( meson.project_name(),
                                ucl_src,
                                dependencies: ucl_deps,
                                install: true,
                                install_dir: get_option('path_install')
                              )

    ucl_dep = declare_dependency (
                include_directories: ucl_inc,
                link_with: ucl_lib
            )    
else
    ucl_lib = shared_library ( meson.project_name(),
                                ucl_src,
                                dependencies: ucl_deps,
                                install: true
                              )

endif
