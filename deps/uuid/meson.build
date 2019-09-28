project ('uuid', 'c',
         version: '1.0.3',
         license: 'BSD',
         default_options:[
         ],
         meson_version: '>=0.51')

cc = meson.get_compiler('c')
ccid = cc.get_id()
osid = target_machine.system()

c_args   = '''
    -lsocket
'''.split()
ld_args  = []
c_defs   = '''
    -DHAVE_CONFIG_H
    -D_DEFAULT_SOURCE
'''.split()

uuid_conf = configuration_data()

#TODO another switches
uuid_conf.set_quoted ('PACKAGE', 'libuuid')
uuid_conf.set_quoted ('PACKAGE_NAME', 'libuuid')
uuid_conf.set_quoted ('PACKAGE_STRING', ' '.join(['libuuid', meson.project_version()]))
uuid_conf.set_quoted ('PACKAGE_TARNAME', 'libuuid')
uuid_conf.set_quoted ('PACKAGE_URL', '')
uuid_conf.set_quoted ('PACKAGE_VERSION', meson.project_version())
uuid_conf.set_quoted ('VERSION', meson.project_version())

config_headers = {
    'HAVE_DLFCN_H'      :'dlfcn.h',
    'HAVE_FCNTL_H'      :'fcntl.h',
    'HAVE_INTTYPES_H'   :'inttypes.h',
    'HAVE_LIMITS_H'     :'limits.h',
    'HAVE_MEMORY_H'     :'memory.h',
    'HAVE_NETINET_IN_H' :'netinet/in.h',
    'HAVE_STDINT_H'     :'stdint.h',
    'HAVE_STDLIB_H'     :'stdlib.h',
    'HAVE_STRINGS_H'    :'strings.h',
    'HAVE_STRING_H'     :'string.h',
    'HAVE_SYS_FILE_H'   :'sys/file.h',
    'HAVE_SYS_IOCTL_H'  :'sys/ioctl.h',
    'HAVE_SYS_SOCKET_H' :'sys/socket.h',
    'HAVE_SYS_STAT_H'   :'sys/stat.h',
    'HAVE_SYS_TIME_H'   :'sys/time.h',
    'HAVE_SYS_TYPES_H'  :'sys/types.h',
    'HAVE_UNISTD_H'     :'unistd.h',
}

foreach conf, header : config_headers
    uuid_conf.set10 (conf, cc.has_header(header))
endforeach

config_funcs = {
    'HAVE_FTRUNCATE'    :'ftruncate',
    'HAVE_GETTIMEOFDAY' :'gettimeofday',
    'HAVE_MEMSET'       :'memset',
    'HAVE_SOCKET'       :'socket',
    'HAVE_SRANDOM'      :'srandom',
    'HAVE_STRTOUL'      :'strtoul',
    'HAVE_USLEEP'       :'usleep',
}

foreach conf, func : config_funcs
    uuid_conf.set10 (conf, cc.has_function(func))
endforeach

configure_file (input: 'config.h.in',
                output: 'config.h',
                configuration: uuid_conf)

foreach item : c_args
    if cc.has_argument (item)
        add_project_arguments (item, language: 'c')
    endif
endforeach

foreach item : ld_args
    if cc.has_link_argument (item)
        add_project_link_arguments (item, language: 'c')
    endif
endforeach

foreach item : c_defs
    add_project_arguments (item, language: 'c')
endforeach

uuid_src = '''
    uuid.h
	all-io.h
	c.h
	randutils.h
	clear.c
	compare.c
	copy.c
	gen_uuid.c
	isnull.c
	pack.c
	parse.c
	unpack.c
	unparse.c
	uuidd.h
	uuidd.h
	uuidP.h
	uuid_time.c
	randutils.c
'''.split()

uuid_lib = shared_library (meson.project_name(),
                           uuid_src,
                           install: true)
if meson.is_subproject()
    libuuid = declare_dependency (
                include_directories: '.',
                link_with: uuid_lib)
endif
