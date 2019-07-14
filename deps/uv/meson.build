project ('uv', 'c',
         version: '1.30.2',
         license: 'BSD license',
         default_options:[
            'c_std=gnu89'
         ],
         meson_version: '>=0.49')

cc = meson.get_compiler ('c')

ccid = cc.get_id()
sysid = target_machine.system()

uv_cflags       = []
uv_defines      = []
uv_libraries    = []

if ccid == 'msvc'
    uv_cflags += '''
        /W4
    '''.split()
elif (ccid == 'clang') or (ccid == 'gcc')
    uv_cflags += '''
        -fvisibility=hidden
        --std=gnu89
        -Wall
        -Wextra
        -Wstrict-prototypes
        -Wno-unused-parameter
    '''.split()
endif

uv_source = files ('''
    src/fs-poll.c
    src/idna.c
    src/inet.c
    src/strscpy.c
    src/threadpool.c
    src/timer.c
    src/uv-common.c
    src/uv-data-getter-setters.c
    src/version.c
'''.split())

if sysid == 'windows'
    uv_defines += '''
        -DWIN32_LEAN_AND_MEAN
        -D_WIN32_WINNT=0x0600
    '''.split()
    uv_libraries+= '''
       advapi32
       iphlpapi
       psapi
       shell32
       user32
       userenv
       ws2_32
    '''.split()
    uv_source += files ('''
       src/win/async.c
       src/win/core.c
       src/win/detect-wakeup.c
       src/win/dl.c
       src/win/error.c
       src/win/fs.c
       src/win/fs-event.c
       src/win/getaddrinfo.c
       src/win/getnameinfo.c
       src/win/handle.c
       src/win/loop-watcher.c
       src/win/pipe.c
       src/win/thread.c
       src/win/poll.c
       src/win/process.c
       src/win/process-stdio.c
       src/win/signal.c
       src/win/snprintf.c
       src/win/stream.c
       src/win/tcp.c
       src/win/tty.c
       src/win/udp.c
       src/win/util.c
       src/win/winapi.c
       src/win/winsock.c  
    '''.split())
else
    uv_defines += '''
        -D_FILE_OFFSET_BITS=64
        -D_LARGEFILE_SOURCE
    '''.split()
    if not (sysid == 'android')
        uv_libraries += '''
            pthread
        '''.split()
    endif
    uv_source += files ('''
       src/unix/async.c
       src/unix/core.c
       src/unix/dl.c
       src/unix/fs.c
       src/unix/getaddrinfo.c
       src/unix/getnameinfo.c
       src/unix/loop-watcher.c
       src/unix/loop.c
       src/unix/pipe.c
       src/unix/poll.c
       src/unix/process.c
       src/unix/signal.c
       src/unix/stream.c
       src/unix/tcp.c
       src/unix/thread.c
       src/unix/tty.c
       src/unix/udp.c
    '''.split())
endif

if sysid == 'android'
    uv_libraries = '''
        dl
    '''.split()
    uv_source += files ('''
       src/unix/android-ifaddrs.c
       src/unix/linux-core.c
       src/unix/linux-inotify.c
       src/unix/linux-syscalls.c
       src/unix/procfs-exepath.c
       src/unix/pthread-fixes.c
       src/unix/sysinfo-loadavg.c
    '''.split())
endif

foreach id : ['android','linux']
    if id == sysid
        uv_source += files ('src/unix/proctitle.c')
    endif
endforeach

foreach id : ['dragonfly','freebsd']
    if id == sysid
        uv_source += files ('src/unix/freebsd.c')
    endif
endforeach

foreach id : ['dragonfly','freebsd','netbsd','openbsd']
    if id == sysid
        uv_sources += files ('''
            src/unix/posix-hrtime.c
            src/unix/bsd-proctitle.c
        '''.split())
        uv_libraries += 'kvm'
    endif
endforeach

foreach id : ['darwin', 'dragonfly','freebsd','netbsd','openbsd']
    if id == sysid
        uv_sources += files ('''
            src/unix/bsd-ifaddrs.c
            src/unix/kqueue.c
        '''.split())
    endif
endforeach

foreach id : ['darwin']
    if id == sysid
        uv_defines += '''
            -D_DARWIN_UNLIMITED_SELECT=1
            -D_DARWIN_USE_64_BIT_INODE=1     
        '''.split()
        uv_sources += files ('''
            src/unix/darwin-proctitle.c
            src/unix/darwin.c
            src/unix/fsevents.c
        '''.split())
    endif
endforeach

foreach id : ['linux']
    if id == sysid
        uv_defines += '''
            -D_GNU_SOURCE
            -D_POSIX_C_SOURCE=200112
        '''.split()
        uv_libraries += '''
            dl
            rt
        '''.split()
        uv_source += files ('''
               src/unix/linux-core.c
               src/unix/linux-inotify.c
               src/unix/linux-syscalls.c
               src/unix/procfs-exepath.c
               src/unix/sysinfo-loadavg.c
        '''.split())
    endif
endforeach

foreach id : ['netbsd']
    if id == sysid
        uv_source += files ('src/unix/netbsd.c')
    endif
endforeach

foreach id : ['openbsd']
    if id == sysid
        uv_source += files ('src/unix/openbsd.c')
    endif
endforeach

if ccid == 'msvc'
    uv_defines += '/DBUILDING_UV_SHARED=1'
else
    uv_defines += '-DBUILDING_UV_SHARED=1'
endif

# Apply all arg-lists
foreach def : uv_defines
    add_project_arguments (def, language: 'c')
endforeach
foreach arg : uv_cflags
    if cc.has_argument(arg)
        add_project_arguments (def, language: 'c')
    endif
endforeach
uv_dep = []
foreach dep : uv_libraries
    uv_dep += cc.find_library(dep, required: true)
endforeach

uv_includes = include_directories ([
    '.'/'include',
    '.'/'src'
])

# Build binaries
if meson.is_subproject()
    uv_lib_shared = shared_library (meson.project_name(),
                                    uv_source,
                                    dependencies: uv_dep,
                                    include_directories: uv_includes,
                                    install: true,
                                    install_dir: get_option('path_install'))
    uv_dep = declare_dependency (
                include_directories: uv_includes,
                link_with: uv_lib_shared
            )
    uv_header = files('.'/'include'/'uv.h') 
else
    uv_lib_shared = shared_library (meson.project_name(),
                                    uv_source,
                                    dependencies: uv_dep,
                                    include_directories: uv_includes,
                                    install: true)
endif
