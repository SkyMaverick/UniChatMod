project ('mdbx', 'c',
         version: '0.2.0',
         license: 'ReOpenLDAP',
         default_options:[
            'c_std=gnu11'
         ],
         meson_version: '>=0.46')

cc = meson.get_compiler('c')

# ==================================================
#   SOURCE FILES
# ==================================================
mdbx_header = files ('mdbx.h')

mdbx_src = '''
    bits.h
    defs.h
    mdbx.c
    osal.c
    osal.h
    version.c
'''.split()

if target_machine.system() == 'windows'
    message ('Use lck-windows functionality on @0@ platform'.format (host_machine.system()) )
    mdbx_src += ['lck-windows.c']
else
    message ('Use lck-posix functionality on @0@ platform'.format (host_machine.system()) )
    mdbx_src += ['lck-posix.c']
endif

mdbx_real_src = mdbx_header
foreach item : mdbx_src
    mdbx_real_src += join_paths('src', item)
endforeach

# ==================================================
#   BUILD ARGUMENTS
# ==================================================
add_project_arguments ('-DNDEBUG=1', language: 'c')
add_project_arguments ('-DMDBX_DEBUG=0', language: 'c')
add_project_arguments ('-DLIBMDBX_EXPORTS=1', language: 'c')
add_project_arguments ('-D_GNU_SOURCE=1', language: 'c')

cc_args = '''
    -O2
    -g3
    -Wall
    -Werror
    -Wextra
    -ffunction-sections
    -fvisibility=hidden
'''.split()

cc_link_args = '''
    -Wl,--gc-sections
    -z,relro
    -O
    -Wl,--no-as-needed
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

mdbx_deps = []
if not (target_machine.system() == 'windows')
    mdbx_deps += [
        cc.find_library ('pthread', required: true),
        cc.find_library ('rt', required: true)
    ]
endif

mdbx_inc = include_directories('.')

if meson.is_subproject()
    message ('install dir: @0@'.format (get_option('path_install')))

    mdbx_lib = shared_library ( meson.project_name(),
                                mdbx_real_src,
                                dependencies: mdbx_deps,
                                install: true,
                                install_dir: get_option('path_install')
                              )

    libmdbx_dep = declare_dependency (
                include_directories: mdbx_inc,
                link_with: mdbx_lib
            )    
else
    mdbx_lib = shared_library ( meson.project_name(),
                                mdbx_real_src,
                                dependencies: mdbx_deps,
                                install: true
                              )

endif
