if PLIBSYS_ARCH_BITS == 'unknown'
    error ('Failed to detect bitness of the target system')
endif

PLIBSYS_NATIVE_WINDOWS = false
if (PLIBSYS_TARGET_PLATFORM == 'windows')
    PLIBSYS_NATIVE_WINDOWS = true
endif

message ('Select platform: @0@'.format(PLIBSYS_TARGET_PLATFORM))

if PLIBSYS_COVERAGE
    if (PLIBSYS_C_COMPILER == 'gcc') or (PLIBSYS_C_COMPILER == 'clang')
        # TODO check buildtype to debug. I don't know
        add_project_arguments ('--coverage', language: 'c')
    endif
endif


# Include target platform definitions
subdir ('platform'/PLIBSYS_TARGET_PLATFORM)

# include directories
plibsys_include = include_directories([
    '.'
])

# ##################################################
#   DEFINE BUILD FILES GROUPS
# ##################################################

PLIBSYS_PUBLIC_HDRS = files ('''
        patomic.h
        ptypes.h
        pmacros.h
        pmacroscompiler.h
        pmacroscpu.h
        pmacrosos.h
        pcondvariable.h
        pcryptohash.h
        perror.h
        perrortypes.h
        pdir.h
        pfile.h
        phashtable.h
        pinifile.h
        plibsys.h
        plibraryloader.h
        plist.h
        pmain.h
        pmem.h
        pmutex.h
        pprocess.h
        prwlock.h
        psemaphore.h
        pshm.h
        pshmbuffer.h
        psocket.h
        psocketaddress.h
        pspinlock.h
        pstdarg.h
        pstring.h
        ptimeprofiler.h
        ptree.h
        puthread.h
'''.split())

PLIBSYS_PRIVATE_HDRS = files ('''
        pcryptohash-gost3411.h
        pcryptohash-md5.h
        pcryptohash-sha1.h
        pcryptohash-sha2-256.h
        pcryptohash-sha2-512.h
        pcryptohash-sha3.h
        perror-private.h
        plibsys-private.h
        psysclose-private.h
        ptimeprofiler-private.h
        ptree-avl.h
        ptree-bst.h
        ptree-rb.h
        ptree-private.h
        puthread-private.h
'''.split())
#TODO add config

PLIBSYS_SRCS = files ('''
        pcryptohash.c
        pcryptohash-gost3411.c
        pcryptohash-md5.c
        pcryptohash-sha1.c
        pcryptohash-sha2-256.c
        pcryptohash-sha2-512.c
        pcryptohash-sha3.c
        pdir.c
        perror.c
        pfile.c
        phashtable.c
        pinifile.c
        plist.c
        pmain.c
        pmem.c
        pprocess.c
        pshmbuffer.c
        psocket.c
        psocketaddress.c
        pstring.c
        ptimeprofiler.c
        ptree.c
        ptree-avl.c
        ptree-bst.c
        ptree-rb.c
        puthread.c
'''.split())

if PLIBSYS_NATIVE_WINDOWS == true
    PLIBSYS_CLOSE_MODEL = 'win'
elif PLIBSYS_TARGET_OS == 'darwin'
    PLIBSYS_CLOSE_MODEL = 'darwin'
else
    PLIBSYS_CLOSE_MODEL = 'unix'
endif

if PLIBSYS_THREAD_MODEL == ''
    PLIBSYS_THREAD_MODEL = 'none'
endif

if PLIBSYS_IPC_MODEL == ''
    PLIBSYS_IPC_MODEL = 'none'
endif

if PLIBSYS_TIME_PROFILER_MODEL == ''
    PLIBSYS_TIME_PROFILER_MODEL = 'generic'
endif

if PLIBSYS_DIR_MODEL == ''
    PLIBSYS_DIR_MODEL = 'none'
endif

if PLIBSYS_LIBRARYLOADER_MODEL == ''
    PLIBSYS_LIBRARYLOADER_MODEL = 'none'
endif

PLIBSYS_PLATFORM_SRCS = [
    ''.join(['pcondvariable','-',PLIBSYS_THREAD_MODEL,'.c']),
    ''.join(['pmutex','-',PLIBSYS_THREAD_MODEL,'.c']),
    ''.join(['psemaphore','-',PLIBSYS_IPC_MODEL,'.c']),
    ''.join(['pshm','-',PLIBSYS_IPC_MODEL,'.c']),
    ''.join(['psysclose','-',PLIBSYS_CLOSE_MODEL,'.c']),
    ''.join(['puthread','-',PLIBSYS_THREAD_MODEL,'.c']),
    ''.join(['ptimeprofiler','-',PLIBSYS_TIME_PROFILER_MODEL,'.c']),
    ''.join(['pdir','-',PLIBSYS_DIR_MODEL,'.c']),
    ''.join(['plibraryloader','-',PLIBSYS_LIBRARYLOADER_MODEL,'.c']),
]

if not (PLIBSYS_IPC_MODEL == 'none')
    PLIBSYS_PRIVATE_HDRS += 'pipc-private.h'
    PLIBSYS_SRCS += 'pipc.c'
endif

PLIBSYS_ATOMIC_LOCK_FREE = false
PLIBSYS_ATOMIC_MODEL = false # TODO
if (not PLIBSYS_ATOMIC_MODEL)
    message ('Checking for lock-free atomic intrinsics')
    if not (PLIBSYS_C_COMPILER == 'clang')
        test_code = '''
            int main () {
                int i, tmp_int = 0;
                void *ptr, *tmp_ptr = 0;
                __atomic_store_4 (&i, 0, __ATOMIC_SEQ_CST);
                __atomic_compare_exchange_n (&i, &tmp_int, 1, 0,
                                   __ATOMIC_SEQ_CST, __ATOMIC_SEQ_CST);
                __atomic_compare_exchange_n ((unsigned long long *) &ptr,
                                   (unsigned long long *) &tmp_ptr,
                                   (unsigned long long) 1, 0,
                                   __ATOMIC_SEQ_CST, __ATOMIC_SEQ_CST);
                __atomic_fetch_add (&i, 1, __ATOMIC_SEQ_CST);
                return 0;
            }

        '''
        if cc.run (test_code).compiled()
            PLIBSYS_ATOMIC_LOCK_FREE = true
            PLIBSYS_ATOMIC_MODEL = 'c11'
        endif
    endif
    if not (PLIBSYS_ATOMIC_LOCK_FREE)
        test_code = '''
            int main () {
                int i;
                __sync_synchronize ();
                __sync_val_compare_and_swap (&i, 0, 1);
                __sync_fetch_and_add (&i, 1);
                return 0;
            }
        '''
        if cc.run (test_code).compiled()
            PLIBSYS_ATOMIC_LOCK_FREE = true
            PLIBSYS_ATOMIC_MODEL = 'sync'
        endif
    endif
    if not (PLIBSYS_ATOMIC_LOCK_FREE)
        if  (PLIBSYS_TARGET_OS == 'windows') and (
                (PLIBSYS_C_COMPILER == 'borland') or
                (PLIBSYS_C_COMPILER == 'watcom')  or
                (PLIBSYS_C_COMPILER == 'icc')     or
                (PLIBSYS_C_COMPILER == 'msvc')
             )
            PLIBSYS_ATOMIC_LOCK_FREE = true
            PLIBSYS_ATOMIC_MODEL = 'win'
        else
            PLIBSYS_ATOMIC_LOCK_FREE = false
            PLIBSYS_ATOMIC_MODEL = 'sim'
        endif
    endif
    if (PLIBSYS_ATOMIC_LOCK_FREE)
            message ('Checking for lock-free atomic intrinsics - works')
    else
            message ('Checking for lock-free atomic intrinsics - not works')
    endif
endif

PLIBSYS_SRCS += files(
    ''.join ( ['patomic-',PLIBSYS_ATOMIC_MODEL,'.c'] ),
    ''.join ( ['pspinlock-',PLIBSYS_ATOMIC_MODEL,'.c'] ),
)

# ##################################################
#   APPLY PLATFORM-SPECIFIC ARGUMENTS
# ##################################################

foreach item : PLIBSYS_PLATFORM_DEFINES
    add_project_arguments (item, language: 'c')
endforeach

PLIBSYS_PLATFORM_DEPS = []
foreach item : PLIBSYS_PLATFORM_LINK_LIBRARIES
    PLIBSYS_PLATFORM_DEPS += cc.find_library (item, required: 'true')
endforeach

foreach item : PLIBSYS_PLATFORM_LDFLAGS
    if cc.has_link_argument(item)
        add_project_link_arguments (item, language: 'c')
    endif
endforeach

# ##################################################
#   CREATE CONFIGURATION DATA
# ##################################################

if target_machine.endian() == 'big'
    PLIBSYS_IS_BIGENDIAN = true
else
    PLIBSYS_IS_BIGENDIAN = false
endif

PLIBSYS_HAVE_FLOAT_H = cc.has_header ('float.h')
PLIBSYS_HAVE_VALUES_H = cc.has_header ('values.h')
PLIBSYS_HAVE_LIMITS_H = cc.has_header ('limits.h')

plibsys_conf = configuration_data()

plibsys_conf.set ('PLIBSYS_VERSION_MAJOR', meson.project_version()[0])
plibsys_conf.set ('PLIBSYS_VERSION_MINOR', meson.project_version()[1])
plibsys_conf.set ('PLIBSYS_VERSION_PATCH', meson.project_version()[2])
plibsys_conf.set ('PLIBSYS_VERSION', meson.project_version())
plibsys_conf.set ('PLIBSYS_VERSION_NUM', meson.project_version())

plibsys_conf. set ('PLIBSYS_SIZEOF_VOID_P_CODE', PLIBSYS_SIZEOF_VOID_P)
plibsys_conf. set ('PLIBSYS_SIZEOF_SIZE_T_CODE', PLIBSYS_SIZEOF_SIZE_T)
plibsys_conf. set ('PLIBSYS_SIZEOF_LONG_CODE'  , PLIBSYS_SIZEOF_LONG)


if PLIBSYS_HAVE_FLOAT_H
    plibsys_conf.set10 ('PLIBSYS_NEED_FLOAT_H', true)
    plibsys_conf.set   ('PLIBSYS_FLOAT_MIN', 'FLT_MIN')
    plibsys_conf.set   ('PLIBSYS_FLOAT_MAX', 'FLT_MAX')
    plibsys_conf.set   ('PLIBSYS_DOUBLE_MIN', 'DBL_MIN')
    plibsys_conf.set   ('PLIBSYS_DOUBLE_MAX', 'DBL_MAX')
elif PLIBSYS_HAVE_VALUES_H
    plibsys_conf.set10 ('PLIBSYS_NEED_VALUES_H', true)
    plibsys_conf.set   ('PLIBSYS_FLOAT_MIN', 'MINFLOAT')
    plibsys_conf.set   ('PLIBSYS_FLOAT_MAX', 'MAXFLOAT')
    plibsys_conf.set   ('PLIBSYS_DOUBLE_MIN', 'MINDOUBLE')
    plibsys_conf.set   ('PLIBSYS_DOUBLE_MAX', 'MAXDOUBLE')
endif

if PLIBSYS_HAVE_LIMITS_H
    plibsys_conf.set10 ('PLIBSYS_NEED_LIMITS_H', true)
    plibsys_conf.set   ('PLIBSYS_SHORT_MIN', 'SHRT_MIN')
    plibsys_conf.set   ('PLIBSYS_SHORT_MAX', 'SHRT_MAX')
    plibsys_conf.set   ('PLIBSYS_USHORT_MAX', 'USHRT_MAX')
    plibsys_conf.set   ('PLIBSYS_INT_MIN', 'INT_MIN')
    plibsys_conf.set   ('PLIBSYS_INT_MAX', 'INT_MAX')
    plibsys_conf.set   ('PLIBSYS_UINT_MAX', 'UINT_MAX')
    plibsys_conf.set   ('PLIBSYS_LONG_MIN', 'LONG_MIN')
    plibsys_conf.set   ('PLIBSYS_LONG_MAX', 'LONG_MAX')
    plibsys_conf.set   ('PLIBSYS_ULONG_MAX', 'ULONG_MAX')
elif
    plibsys_conf.set10 ('PLIBSYS_NEED_VALUES_H', true)
    plibsys_conf.set   ('PLIBSYS_SHORT_MIN', 'MINSHORT')
    plibsys_conf.set   ('PLIBSYS_SHORT_MAX', 'MAXSHORT')
    plibsys_conf.set   ('PLIBSYS_USHORT_MAX', '(((pushort) P_MAXSHORT) * 2 + 1)')
    plibsys_conf.set   ('PLIBSYS_INT_MIN', 'MININT')
    plibsys_conf.set   ('PLIBSYS_INT_MAX', 'MAXINT')
    plibsys_conf.set   ('PLIBSYS_UINT_MAX', '(((puint) P_MAXINT) * 2 + 1)')
    plibsys_conf.set   ('PLIBSYS_LONG_MIN', 'MINLONG')
    plibsys_conf.set   ('PLIBSYS_LONG_MAX', 'MAXLONG')
    plibsys_conf.set   ('PLIBSYS_ULONG_MAX', '(((pulong) P_MAXLONG) * 2 + 1)')
endif

if PLIBSYS_NATIVE_WINDOWS
    plibsys_conf.set ('PLIBSYS_NEED_WINDOWS_H', true)
endif

if not PLIBSYS_NATIVE_WINDOWS
    # Check for anonymous mmap()
    message ('Checking whether mmap has anonymous mapping')
    test_code_anon = '''
        #include <sys/types.h>
        #include <sys/mman.h>
        int main () {
            mmap (0, 1024, PROT_READ | PROT_WRITE, MAP_ANON | MAP_PRIVATE, -1, 0);
            return 0;
        }
    '''
    test_code_anonymous = '''
        #include <sys/types.h>
        #include <sys/mman.h>
        int main () {
            mmap (0, 1024, PROT_READ | PROT_WRITE, MAP_ANONYMOUS | MAP_PRIVATE, -1, 0);
            return 0;
        }
    '''
    PLIBSYS_MMAP_HAS_MAP_ANON = cc.run(test_code_anon).compiled()
    PLIBSYS_MMAP_HAS_MAP_ANONYMOUS = cc.run(test_code_anonymous).compiled()
    if PLIBSYS_MMAP_HAS_MAP_ANON or PLIBSYS_MMAP_HAS_MAP_ANONYMOUS
        message ('Checking whether mmap has anonymous mapping - yes')
    else
        message ('Checking whether mmap has anonymous mapping - no')
    endif
    if PLIBSYS_MMAP_HAS_MAP_ANONYMOUS
        add_project_arguments ('-DPLIBSYS_MMAP_HAS_MAP_ANONYMOUS', language: 'c')
    elif PLIBSYS_MMAP_HAS_MAP_ANON
        add_project_arguments ('-DPLIBSYS_MMAP_HAS_MAP_ANON', language: 'c')
    endif

#    # Check for clock_nanosleep() call
    test_code_cns = '''
         #include <time.h>
         int main () {
                struct timespec time_sp = {0, 500000000L};
                clock_nanosleep (CLOCK_MONOTONIC, 0, &time_sp, NULL);
                return 0;
         }
    '''
    test_code_ns = '''
         #include <time.h>
         int main () {
                struct timespec time_sp = {0, 500000000L};
                nanosleep (&time_sp, NULL);
                return 0;
         }
    '''
    message ('Checking whether clock_nanosleep() presents')
    if cc.run(test_code_cns).compiled()
        message ('Checking whether clock_nanosleep() presents - yes')
        add_project_arguments ('-DPLIBSYS_HAS_CLOCKNANOSLEEP', language: 'c')
    else
        message ('Checking whether clock_nanosleep() presents - no')
        message ('Checking whether nanosleep() presents')
        if cc.run(test_code_ns).compiled()
            message ('Checking whether nanosleep() presents - yes')
            add_project_arguments ('-DPLIBSYS_HAS_NANOSLEEP', language: 'c')
        else
            message ('Checking whether nanosleep() presents - no')
        endif
    endif

    #Check for getaddrinfo() call
    message ('Checking whether getaddrinfo() presents')
    test_code = '''
         #include <sys/socket.h>
         #include <netdb.h>
         int main () {
                getaddrinfo (0, 0, 0, 0);
                freeaddrinfo (0);

                return 0;
         }
    '''
    if cc.run(test_code).compiled()
        message ('Checking whether getaddrinfo() presents - yes')
        add_project_arguments ('-DPLIBSYS_HAS_GETADDRINFO', language: 'c')
    else
        message ('Checking whether getaddrinfo() presents - no')
    endif
endif

PLIBSYS_RWLOCK_MODEL = PLIBSYS_THREAD_MODEL
if (PLIBSYS_THREAD_MODEL == 'posix')
    if (PLIBSYS_TARGET_OS == 'derwin') or (PLIBSYS_TARGET_OS == 'aix')
        PLIBSYS_SCHED_CHECK = 'defined (_POSIX_THREAD_PRIORITY_SCHEDULING)'
    elif PLIBSYS_TARGET_OS == 'android'
        PLIBSYS_SCHED_CHECK = '0'
    else
        PLIBSYS_SCHED_CHECK = 'defined (_POSIX_THREAD_PRIORITY_SCHEDULING) && (_POSIX_THREAD_PRIORITY_SCHEDULING - 0 >= 0)'
    endif

    pthread_deps = cc.find_library ('pthread', required: 'true')
    message ('Checking whether POSIX thread scheduling presents')
    test_code = '''
         #include <unistd.h>
         #include <pthread.h>
         #include <sched.h>

         int main () {
                #if @0@
                  sched_get_priority_min (0);
                  sched_get_priority_max (0);
                #else
                  stop_compile_here
                #endif
                return 0;
         }
    '''.format(PLIBSYS_SCHED_CHECK)
    if cc.run(test_code, dependencies: pthread_deps).compiled()
        message ('Checking whether POSIX thread scheduling presents - yes')
        add_project_arguments ('-DPLIBSYS_HAS_POSIX_SCHEDULING', language: 'c')
    else
        message ('Checking whether POSIX thread scheduling presents - no')
    endif

    # Check for thread stack size
    message ('Checking whether POSIX thread stack size is supported')
    test_code = '''
         #include <pthread.h>

         int main () {
                pthread_attr_t attr;
                pthread_attr_setstacksize (&attr, 0);
                return 0;
         }
    '''
    if cc.run(test_code, dependencies: pthread_deps).compiled()
        message ('Checking whether POSIX thread stack size is supported - yes')
        add_project_arguments ('-DPLIBSYS_HAS_POSIX_STACKSIZE', language: 'c')
    else
        message ('Checking whether POSIX thread stack size is supported - no')
    endif
endif

if PLIBSYS_THREAD_MODEL == 'posix' and PLIBSYS_RWLOCK_MODEL == 'posix'
    # Check for thread stack size
    message ('Checking whether POSIX thread stack size is supported')
    test_code = '''
         #include <pthread.h>

         int main () {
                pthread_rwlock_t rwl;

                pthread_rwlock_init (&rwl, 0);
                pthread_rwlock_destroy (&rwl);
                return 0;
         }
    '''
    if cc.run(test_code, dependencies: pthread_deps).compiled()
        message ('Checking whether POSIX read-write locks are supported - yes')
    else
        message ('Checking whether POSIX read-write locks are supported - no')
        PLIBSYS_RWLOCK_MODEL = 'general'
    endif
endif
PLIBSYS_PLATFORM_SRCS += ''.join(['prwlock-', PLIBSYS_RWLOCK_MODEL,'.c'])

if PLIBSYS_NATIVE_WINDOWS
    PLIBSYS_SOCKET_INCLUDES = '''
            #include <winsock2.h>
            #include <ws2tcpip.h>
            #include <windows.h>
    '''
else
    PLIBSYS_SOCKET_INCLUDES = '''
            #include <sys/types.h>
            #include <sys/socket.h>
            #include <netinet/in.h>
    '''
endif

# Check for socklen_t definition
message ('Checking whether socklen_t is defined')
test_code = '''
          @0@
          int main () {
                socklen_t len = sizeof (socklen_t);
                return len > 0 ? 0 : -1;
          }
'''.format(PLIBSYS_SOCKET_INCLUDES)

if cc.run(test_code).compiled()
    message ('Checking whether socklen_t is defined - yes')
    add_project_arguments ('-DPLIBSYS_HAS_SOCKLEN_T', language: 'c')
else
    message ('Checking whether socklen_t is defined - no')
endif

# Check for sockaddr_storage structure
message ('Checking whether struct sockaddr_storage is defined')
test_code = '''
            @0@
            int main () {
                struct sockaddr_storage sock_addr;
                sock_addr.ss_family = AF_INET;

                return 0;
            }
'''.format(PLIBSYS_SOCKET_INCLUDES)
if cc.run(test_code).compiled()
        message ('Checking whether struct sockaddr_storage is defined - yes')
        add_project_arguments ('-DPLIBSYS_HAS_SOCKADDR_STORAGE', language: 'c')
else
        message ('Checking whether struct sockaddr_storage is defined - no')
endif

# Check sa_len field in struct sockaddr
message ('Checking whether struct sockaddr has sa_len')
test_code = '''
        @0@
        int main () {
            struct sockaddr sock_addr;
            sock_addr.sa_len = 0;

            return 0;
        }
'''.format(PLIBSYS_SOCKET_INCLUDES)

if cc.run(test_code).compiled()
        message ('Checking whether struct sockaddr has sa_len - yes')
        add_project_arguments ('-DPLIBSYS_SOCKADDR_HAS_SA_LEN', language: 'c')
else
        message ('Checking whether struct sockaddr has sa_len - no')
endif

# Check sin6_scope_id field in struct sockaddr_in6
message ('Checking whether struct sockaddr_in6 has sin6_scope_id')
test_code = '''
        @0@
        int main () {
            struct sockaddr_in6 sock_addr;
            sock_addr.sin6_scope_id = 0;

            return 0;
        }
'''.format(PLIBSYS_SOCKET_INCLUDES)

if cc.run(test_code).compiled()
        message ('Checking whether struct sockaddr_in6 has sin6_scope_id - yes')
        add_project_arguments ('-DPLIBSYS_SOCKADDR_IN6_HAS_SCOPEID', language: 'c')
else
        message ('Checking whether struct sockaddr_in6 has sin6_scope_id - no')
endif

# Check sin6_flowinfo field in struct sockaddr_in6
message ('Checking whether struct sockaddr_in6 has sin6_flowinfo')
test_code = '''
          @0@
          int main () {
                struct sockaddr_in6 sock_addr;
                sock_addr.sin6_flowinfo = 0;

                return 0;
          }
'''.format(PLIBSYS_SOCKET_INCLUDES)

if cc.run(test_code).compiled()
        message ('Checking whether struct sockaddr_in6 has sin6_flowinfo - yes')
        add_project_arguments ('-DPLIBSYS_SOCKADDR_IN6_HAS_FLOWINFO', language: 'c')
else
        message ('Checking whether struct sockaddr_in6 has sin6_flowinfo - no')
endif

PLIBSYS_SIZEOF_SAFAMILY_T = cc.sizeof ('((struct sockaddr *) 0)->sa_family',
                                        prefix: PLIBSYS_SOCKET_INCLUDES)

if PLIBSYS_VISIBILITY
    message ('Checking whether compiler supports visibility')
    if (PLIBSYS_VISIBILITY_CFLAGS == '' and PLIBSYS_VISIBILITY_LDFLAGS == '')
        message ('Checking whether compiler supports visibility - no')
        PLIBSYS_VISIBILITY = false
    else
        message ('Checking whether compiler supports visibility - yes')
        add_project_arguments (PLIBSYS_VISIBILITY_CFLAGS, language: 'c')
        add_project_link_arguments (PLIBSYS_VISIBILITY_LDFLAGS, language: 'c')
    endif
endif

plibsys_conf.set ('PLIBSYS_SIZEOF_SAFAMILY_T', PLIBSYS_SIZEOF_SAFAMILY_T)
plibsys_conf.set ('PLIBSYS_VA_COPY', PLIBSYS_VA_COPY)

# create config file
configure_file (input: 'plibsysconfig-meson.h.in',
                output: 'plibsysconfig.h',
                configuration: plibsys_conf)

# ##################################################
#   FINISH OPERATIONS
# ##################################################
if meson_ccid == 'msvc'
    add_project_arguments ('-D_CRT_SECURE_NO_WARNINGS', language: 'c')
    add_project_arguments ('-D_WINSOCK_DEPRECATED_NO_WARNINGS', language: 'c')
endif

add_project_arguments ('-DPLIBSYS_COMPILATION', language: 'c')
foreach item : PLIBSYS_PLATFORM_DEFINES
    add_project_arguments (item, language: 'c')
endforeach

if PLIBSYS_VA_COPY == ''
    PLIBSYS_VA_COPY_STATUS = false
else
    PLIBSYS_VA_COPY_STATUS = true
endif

if PLIBSYS_ARCH_BITS == 'universal'
    PLIBSYS_ADDRESS_MODEL = 'universal'
else
    PLIBSYS_ADDRESS_MODEL = ' '.join([PLIBSYS_ARCH_BITS,'bit'])
endif

# ##################################################
#   END INFO
# ##################################################
msg_info = '''
        == Build configuration ==

        Platfrom:               @0@
        Compiler:               @1@
        Address model:          @2@

        Thread model:           @3@
        RW lock model:          @4@
        IPC model:              @5@
        DIR model:              @6@
        Library loader model:   @7@
        Time profiler model:    @8@
        Atomic model:           @9@

        Platform defines:       @10@
        Platform CFLAGS:        @11@
        Platform LDFLAGS:       @12@
        Platform libraries:     @13@

        Build static library:   @14@
        Build tests:            @15@
        Coverage support:       @16@
        Visibility:             @17@

        va_copy availability:   @18@
'''.format(PLIBSYS_TARGET_OS,
           PLIBSYS_C_COMPILER,
           PLIBSYS_ADDRESS_MODEL,
           PLIBSYS_THREAD_MODEL,
           PLIBSYS_RWLOCK_MODEL,
           PLIBSYS_IPC_MODEL,
           PLIBSYS_DIR_MODEL,
           PLIBSYS_LIBRARYLOADER_MODEL,
           PLIBSYS_TIME_PROFILER_MODEL,
           PLIBSYS_ATOMIC_MODEL,
           PLIBSYS_PLATFORM_DEFINES,
           PLIBSYS_PLATFORM_CFLAGS,
           PLIBSYS_PLATFORM_LDFLAGS,
           PLIBSYS_PLATFORM_LINK_LIBRARIES,
           PLIBSYS_BUILD_STATIC,
           PLIBSYS_TESTS,
           PLIBSYS_COVERAGE,
           PLIBSYS_VISIBILITY,
           PLIBSYS_VA_COPY_STATUS
           )

message (msg_info)
if meson.is_subproject()
    libplsys = shared_library (meson.project_name(),
                               PLIBSYS_SRCS +
                               PLIBSYS_PLATFORM_SRCS +
                               PLIBSYS_PUBLIC_HDRS +
                               PLIBSYS_PRIVATE_HDRS,
                               dependencies: PLIBSYS_PLATFORM_DEPS,
                               install: 'true',
                               install_dir: get_option('path_install'))
    plibsys_dep = declare_dependency (
                include_directories: plibsys_include,
                link_with: libplsys
            )    
else
    libplsys = shared_library (meson.project_name(),
                               PLIBSYS_SRCS +
                               PLIBSYS_PLATFORM_SRCS +
                               PLIBSYS_PUBLIC_HDRS +
                               PLIBSYS_PRIVATE_HDRS,
                               dependencies: PLIBSYS_PLATFORM_DEPS,
                               install: 'true')
endif
if PLIBSYS_BUILD_STATIC
        libplsys = static_library (meson.project_name(),
                                   PLIBSYS_SRCS +
                                   PLIBSYS_PLATFORM_SRCS +
                                   PLIBSYS_PUBLIC_HDRS +
                                   PLIBSYS_PRIVATE_HDRS,
                                   dependencies: PLIBSYS_PLATFORM_DEPS)
    endif

