project ('mdbx', 'c',
         version: '0.3.1',
         license: 'ReOpenLDAP',
         default_options:[
         ],
         meson_version: '>=0.46')

cc = meson.get_compiler('c')
ccid = cc.get_id()
osid = target_machine.system()

if get_option('buildtype').endswith('debug')
    enable_debug = true
else
    enable_debug = false
endif

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

if osid == 'windows'
    message ('Use lck-windows functionality on @0@ platform'.format (osid))
    mdbx_src += ['lck-windows.c']
elif osid == 'linux'
    message ('Use lck-linux functionality on @0@ platform'.format (osid))
    mdbx_src += ['lck-linux.c']
else
    message ('Use lck-posix functionality on @0@ platform'.format (osid))
    mdbx_src += ['lck-posix.c']
endif

mdbx_real_src = mdbx_header
foreach item : mdbx_src
    mdbx_real_src += join_paths('src', item)
endforeach

# ==================================================
#   BUILD ARGUMENTS
# ==================================================

cc_args = []
cc_link_args = []
cc_defs = []

if ccid == 'msvc'
    cc_defs += '''
        /DLIBMDBX_EXPORTS=1
        /D_GNU_SOURCE=1
    '''.split()
    if enable_debug
        cc_defs  += '/DMDBX_DEBUG=1'
    else
        cc_defs  += '/DMDBX_DEBUG=0'
    endif
else
    cc_defs += '''
        -DLIBMDBX_EXPORTS=1
        -D_GNU_SOURCE=1
    '''.split()
    if enable_debug
        cc_defs  += '-DMDBX_DEBUG=1'
    else
        cc_defs  += '-DMDBX_DEBUG=0'
    endif
endif

if (ccid == 'gcc') or (ccid == 'clang')
    # Build on linux (or difffernt *nix) system
    if enable_debug
        cc_args = '''
            -std=gnu11
            -O0
            -g
            -Wall
            -Werror
            -Wextra
            -ffunction-sections
            -fvisibility=hidden
        '''.split()
    else
        cc_args = '''
            -std=gnu11
            -O2
            -g3
            -Wall
            -Werror
            -Wextra
            -ffunction-sections
            -fvisibility=hidden
        '''.split()

    endif

    cc_link_args = '''
        -Wl,--gc-sections
        -z,relro
        -O1
        -Wl,--no-as-needed
    '''.split()
elif ccid == 'msvc'
    # Build on Windows with native Visual Studio
    cc_args = '''
    '''.split()

    cc_link_args = '''
    '''.split()
else
    warning ('Unkonown compiler name: @0@ . No use flags.'.format(ccid))
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

mdbx_deps = []
if not (osid == 'windows')
    mdbx_deps += [
        cc.find_library ('pthread', required: true),
        cc.find_library ('rt', required: true)
    ]
endif

# ==================================================
#   INCLUDES AND TARGETS
# ==================================================

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
