SDL2_name = 'sdl2ui'
SDL2_src = files('''
    sdl2ui.c
'''.split()) + shared_headers

SDL2_deps = [
    libucm_depends,
    system_dependencies
]

# 'find_library/subproject name' : 'dependency in subproject'
SDL2_matrix = {
    'SDL2'      : {
        'name'      : 'SDL2',
        'version'   : '>=2.0.8',
        'depend'    : 'SDL2'
    },
    'SDL2_image': {
        'name'      : 'SDL2_image',
        'version'   : '>=2.0.3',
        'depend'    : 'SDL2_image'
    },
    'SDL2_mixer': {
        'name'      : 'SDL2_mixer',
        'version'   : '>=2.0.2',
        'depend'    : 'SDL2_mixer'
    },
    'SDL2_ttf'  : {
        'name'      : 'SDL2_ttf',
        'version'   : '>=2.0.14',
        'depend'    : 'SDL2_ttf'
    }
}

cmake = import ('cmake')

foreach lib, opts: SDL2_matrix
    SDL2_dep = disabler()
    if not get_option('custom_libs')
        SDL2_dep = dependency (opts.get('name'), version: opts.get('version'), required: false,
                not_found_message: 'Don\'t found dependency. Try @0@ for found library, but not guaranteed it correct version'.format(ccid))
        if not SDL2_dep.found()
            SDL2_dep = cc.find_library (lib, required: false)
        endif
    endif
    if not SDL2_dep.found()
        SDL2_dep = cmake.subproject(lib).dependency(opts.get('depend'))
    endif

    SDL2_deps += SDL2_dep
endforeach

SDL2 = shared_library (SDL2_name,
             SDL2_src,
             dependencies: SDL2_deps,
             build_rpath: libs_rpath_plugin,
             install_rpath: libs_rpath_plugin,
             install_dir: install_path_plugin,
             install: true
         )
