project ('uv', 'c',
         version: '1.24.1',
         license: 'BSD license',
         default_options:[
            'c_std=gnu89'
         ],
         meson_version: '>=0.49')

cc = meson.get_compiler('c')

uv_header = files ('include'/'uv.h')
uv_src = []
uv_deps = []

uv_core_src = '''
    fs-poll.c
    idna.c
    inet.c
    strscpy.c
    threadpool.c
    timer.c
    uv-common.c
    uv-data-getter-setters.c
    version.c
'''.split()

foreach file : uv_core_src
    uv_src += files ( 'src'/ file )
endforeach

if target_machine.system() == 'windows'
    add_project_arguments ('-DWIN32_LEAN_AND_MEAN', language: 'c')
    add_project_arguments ('-D_WIN32_WINNT=0x0600', language: 'c')

    uv_deps = [
       cc.find_library('advapi32',  required: 'true'),
       cc.find_library('iphlpapi',  required: 'true'),
       cc.find_library('psapi',     required: 'true'),
       cc.find_library('shell32',   required: 'true'),
       cc.find_library('user32',    required: 'true'),
       cc.find_library('userenv',   required: 'true'),
       cc.find_library('ws2_32',    required: 'true')
    ]
    
    uv_sysdep_src = '''       
       async.c
       core.c
       detect-wakeup.c
       dl.c
       error.c
       fs.c
       fs-event.c
       getaddrinfo.c
       getnameinfo.c
       handle.c
       loop-watcher.c
       pipe.c
       thread.c
       poll.c
       process.c
       process-stdio.c
       signal.c
       snprintf.c
       stream.c
       tcp.c
       tty.c
       udp.c
       util.c
       winapi.c
       winsock.c
    '''.split()
    foreach file : uv_sysdep_src
        uv_src += files ( 'src'/'win'/file )
    endforeach

else
    add_project_arguments ('-D_FILE_OFFSET_BITS=64', language: 'c')
    add_project_arguments ('-D_LARGEFILE_SOURCE', language: 'c')
    
    uv_sysdep_src = '''
       async.c
       core.c
       dl.c
       fs.c
       getaddrinfo.c
       getnameinfo.c
       loop-watcher.c
       loop.c
       pipe.c
       poll.c
       process.c
       signal.c
       stream.c
       tcp.c
       thread.c
       tty.c
       udp.c
    '''.split()
    if target_machine.system() == 'linux'
        uv_deps += [
            cc.find_library('dl', required: 'true'),
            cc.find_library('rt', required: 'true'),
            cc.find_library('pthread', required: 'true')
        ]
        uv_sysdep_src += '''
           linux-core.c
           linux-inotify.c
           linux-syscalls.c
           procfs-exepath.c
           sysinfo-loadavg.c
           sysinfo-memory.c
        '''.split()
    endif
    
    foreach file : uv_sysdep_src
        uv_src += files ( 'src'/'unix'/file )
    endforeach
endif

# ==================================================
#   BUILD ARGUMENTS
# ==================================================
cc_name = cc.get_id()
if (cc_name == 'gcc') or (cc_name == 'clang')
    # Build on linux (or difffernt *nix) system
    cc_args = '''
        -std=gnu89
        -Wall
        -Wextra
        -Wstrict-prototypes
        -Wno-unused-parameter
        -fvisibility=hidden
    '''.split()

    cc_link_args = '''
    '''.split()
elif cc_name == 'msvc'
    # Build on Windows with native Visual Studio
    cc_args = '''
        /W4
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


# ==================================================
#   INCLUDES AND TARGETS
# ==================================================

uv_inc = include_directories('.'/'include', '.'/'src')


add_project_arguments ('-DBUILDING_UV_SHARED=1', language: 'c')
if meson.is_subproject()
    message ('install dir: @0@'.format (get_option('path_install')))

    uv_lib = shared_library ( meson.project_name(),
                                uv_src,
                                dependencies: uv_deps,
                                include_directories: uv_inc,
                                install: true,
                                install_dir: get_option('path_install')
                              )

    uv_dep = declare_dependency (
                include_directories: uv_inc,
                link_with: uv_lib
            )    
else
    uv_lib = shared_library ( meson.project_name(),
                                uv_src,
                                dependencies: uv_deps,
                                include_directories: uv_inc,
                                install: true
                              )

endif
