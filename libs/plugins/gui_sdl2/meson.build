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
    'SDL2' : 'SDL2',
    'SDL2_image': 'SDL2_image',
    'SDL2_mixer': 'SDL2_mixer',
    'SDL2_ttf'  : 'SDL2_ttf'
}

cmake = import ('cmake')

foreach dep, target : SDL2_matrix
    if not get_option('custom_libs')
        SDL2_dep = cc.find_library(dep, required: false)
        if not SDL2_dep.found()            
            SDL2dep_lib = cmake.subproject(dep)
            SDL2_dep = SDL2dep_lib.dependency(target)
        endif
    else
        SDL2dep_lib = cmake.subproject(dep)
        SDL2_dep = SDL2dep_lib.dependency(target)
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
