add_project_arguments ('-DXCURSES', language: 'c')
pdc_x11config = configuration_data()

# === check system headers ==========

pdc_x11headers = '''
    fcntl.h
	sys/time.h
	sys/select.h
	dlfcn.h
    signal.h
'''.split()

foreach item : pdc_x11headers
    if not cc.has_header(item)
        error ('Don\'t found header: @0@'.format(item))
    endif
endforeach

pdc_x11config.set ('PDC_MAX_SIGNALS',
    cc.run( '''
        #include <signal.h>
        int main() {
            return NSIG;
        }''').returncode()
)
if cc.has_header('unistd.h')
    pdc_x11config.set10 ('HAVE_UNISTD_H', true)
    if cc.has_function('usleep', prefix: '#include <unistd.h>')
        pdc_x11config.set10 ('HAVE_USLEEP', true)
    endif
endif
if cc.has_function('vsnprintf', prefix: '#include <stdio.h>')
    pdc_x11config.set10 ('HAVE_VSNPRINTF', true)
endif
if cc.has_function('vsscanf', prefix: '#include <stdio.h>')
    pdc_x11config.set10 ('HAVE_VSSCANF', true)
endif
if cc.has_function('poll', prefix: '#include <poll.h>')
    pdc_x11config.set10 ('HAVE_POLL', true)
endif

if cc.has_header('time.h') and (cc.has_header('sys/time.h'))
    pdc_x11config.set10 ('TIME_WITH_SYS_TIME', true)
endif

# TODO this is hack. Need better validation.
if cc.has_argument('-lsocket')
    add_project_arguments ('-lsocket', language: 'c')
endif
if cc.has_argument('-lnls')
    add_project_arguments ('-lnls', language: 'c')
endif

pdc_deps += [
    dependency('x11',   required: true),
    dependency('xaw7',  required: true),
    dependency('xmu',   required: true),
    dependency('xt',    required: true),
    dependency('sm',    required: true),
    dependency('ice',   required: true),
    dependency('xext',  required: true),
    dependency('xpm',   required: true),
]

# TODO Need better X11 includes path resolve
pdc_x11includes = include_directories('/usr'/'include'/'X11')

if cc.has_header('X11/DECkeysym.h')
    pdc_x11config.set10 ('HAVE_DECKEYSYM_H', true)
endif
if cc.has_header('X11/Sunkeysym.h')
    pdc_x11config.set10 ('HAVE_SUNKEYSYM_H', true)
endif
if cc.has_header('X11/xpm.h')
    pdc_x11config.set10 ('HAVE_XPM_H', true)
endif
if get_option('use_nextaw')
    pdc_x11config.set10 ('USE_NEXTAW', false)
endif
if get_option('use_xaw3d')
    pdc_x11config.set10 ('USE_XAW3D', false)
endif
if cc.run('''
            #include <X11/Xlib.h>
            int main () {
                XPointer a;
            }
          ''').compiled()
    pdc_x11config.set10 ('XPOINTER_TYPEDEFED', true)
endif

# === Build arguments ==========
pdc_args = []

if ccid == 'gcc' or ccid == 'clang'
    pdc_args += '''
        -Wall
        -fomit-frame-pointer
    '''.split()

endif

foreach item : pdc_args
    if cc.has_argument(item)
        add_project_arguments (item, language: 'c')
    endif
endforeach

if get_option('buildtype').endswith('debug')
    add_project_arguments ('-DPDCDEBUG', language: 'c')
endif
if get_option('enable_widec')
    add_project_arguments ('-DPDC_WIDE', language: 'c')
endif
if get_option('enable_force_utf8')
    add_project_arguments ('-DPDC_FORCE_UTF8', language: 'c')
endif
# === configuration file ==========

configure_file (input : 'config_meson.h.in',
                output : 'config.h',
                configuration : pdc_x11config)
add_project_arguments ('-DHAVE_CONFIG_H', language: 'c')

# === build library ==========

pdc_source = pdc_core + files ('''
    pdcclip.c
    pdcdisp.c
    pdcgetsc.c
    pdckbd.c
    pdcscrn.c
    pdcsetsc.c
    pdcutil.c
    pdcx11.h
    sb.c
    ScrollBox.c
    ScrollBox.h
    ScrollBoxP.h
'''.split())

add_project_arguments ('-DDYNAMIC', language: 'c')

if meson.is_subproject()
    pdc_lib_shared = shared_library (meson.project_name(),
                                    pdc_source,
                                    include_directories: [pdc_includes, pdc_x11includes],
                                    dependencies: pdc_deps,
                                    install: true,
                                    install_dir: get_option('path_install'))
    pdcurses_dep = declare_dependency (
                    include_directories: pdc_includes,
                    link_with: pdc_lib_shared
                    )
else
    pdc_lib_shared = shared_library (meson.project_name(),
                                    pdc_source,
                                    include_directories: [pdc_includes, pdc_x11includes],
                                    dependencies: pdc_deps,
                                    install: true)
endif
