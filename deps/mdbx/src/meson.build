# Process version
cfgver = configuration_data()
cfgver.set ('MDBX_VERSION_MAJOR',meson.project_version().split('.')[0])
cfgver.set ('MDBX_VERSION_MINOR',meson.project_version().split('.')[1])
cfgver.set ('MDBX_VERSION_RELEASE',meson.project_version().split('.')[2])
cfgver.set ('MDBX_VERSION_REVISION',meson.project_version().split('.')[3])

if (get_option('MDBX_USE_GIT'))
    cfgver.set ('MDBX_GIT_TIMESTAMP',   run_command(app_git,'describe --tags --long --dirty=-dirty'.split()).stdout().strip())
    cfgver.set ('MDBX_GIT_TREE',        run_command(app_git,'show --no-patch --format=%cI HEAD'.split()).stdout().strip())
    cfgver.set ('MDBX_GIT_COMMIT',      run_command(app_git,'show --no-patch --format=%T HEAD'.split()).stdout().strip())
    cfgver.set ('MDBX_GIT_DESCRIBE',    run_command(app_git,'show --no-patch --format=%H HEAD'.split()).stdout().strip())
    cfgver.set ('MDBX_GIT_REVISION',    run_command(app_git,'rev-list --count --no-merges HEAD'.split()).stdout().strip())
else
    cfgver.set ('MDBX_GIT_TIMESTAMP',   'undefined')
    cfgver.set ('MDBX_GIT_TREE',        'undefined')
    cfgver.set ('MDBX_GIT_COMMIT',      'undefined')
    cfgver.set ('MDBX_GIT_DESCRIBE',    'undefined')
endif

configure_file (
    input: 'elements/version.c.in',
    output: 'version.c',
    configuration: cfgver
)

message ('libmdbx version is @0@'.format(meson.project_version()))

sha256_py = run_command(app_py, '-c', 
'''from hashlib import sha256;
with open("@0@","rb") as f:
    hash = sha256()
    for byte_block in iter(lambda: f.read(4096),b""):
        hash.update(byte_block);
print(hash.hexdigest())'''.format(meson.current_build_dir()/'version.c')).stdout().strip()

if get_option('MDBX_USE_GIT')
    sha256_py += '_' + get_option('MDBX_GIT_COMMIT')
endif

message ('SHA256 for sourcery: @0@'.format(sha256_py))
cfg.set ('MDBX_BUILD_SOURCERY', sha256_py)

libmdbx_src = []

if (get_option('MDBX_ALLOY_MODE'))
    libmdbx_src += 'alloy.c'
else
    libmdbx_src += '''
        elements/defs.h
        elements/internals.h
        elements/core.c
        elements/osal.h
        elements/osal.c
    '''.split()

    libmdbx_src += (osid == 'windows') ? 'elements/lck-windows.c' :
                                         'elements/lck-posix.c'
endif

if osid == 'windows'
    ntdll_def = meson.current_source_dir() / 'elements' / 'ntdll.def'
    if ccid ==  'msvc'
        app_lib = find_program ('lib', required: false)
        if app_lib.found()
            message ('Found MSVC\'s lib tool: @0@'.format(app_lib.path()))
            ntdll_ipmlib = 'mdbx_ntdll_extra.lib'
            ntdll = custom_target ('ntdll_extra',
                        input: ntdll_def,
                        output: ntdll_ipmlib,
                        command: [ app_lib,
                            '/NOLOGO',
                            ':'.join(['/def','@INPUT@']),
                            ':'.join(['/out','@OUTPUT@'])
                        ])
        else
            error ('MSVC\'s lib tool not found')
        endif
    elif ccid == 'gcc'
        app_dtool = find_program ('dlltool', required: false)
        if app_dtool.found()
            message ('Found dlltool: @0@'.format(app_dtool.path()))
            ntdll_ipmlib = meson.build_root() / 'mdbx_ntdll_extra.a'
            ntdll = custom_target ('ntdll_extra',
                        input: ntdll_def,
                        output: ntdll_ipmlib,
                        command: [ app_dtool,
                            '-d', '"@INPUT@"',
                            '-l', '"@OUTPUT@"'
                        ])
        else
            error ('dlltool not found')
        endif
    endif
    mdbx_deps += declare_dependency (link_with: ntdll)
endif

configure_file (
            input: 'elements/config.h.in',
            output: 'config.h',
            configuration: cfg)
add_project_arguments ('-DMDBX_CONFIG_H="config.h"', language: 'c')

if (get_option('MDBX_BUILD_SHARED_LIBRARY'))
    add_project_arguments('-DLIBMDBX_EXPORTS', language: 'c')
    mdbx = shared_library (meson.project_name(),
                libmdbx_src,
                dependencies: mdbx_deps,
                install: true
            )
else
    mdbx = static_library (meson.project_name(),
                libmdbx_src,
                dependencies: mdbx_deps,
                install: true
            )
endif
if meson.is_subproject()
    libmdbx = declare_dependency (
                include_directories: '..',
                link_with: mdbx
            )
endif
