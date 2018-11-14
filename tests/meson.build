test_app_name = 'test_bot'
test_app_src = files('''
    run.c
    util.c
    util.h
'''.split())

test_app_src += [core_header_c]
test_app_utils = [ join_paths(meson.current_source_dir(), 'util.h'),
                   join_paths(meson.current_source_dir(), 'util.c')
                 ]

test_app_deps = [
    cc.find_library('dl', required: true),
    cc.find_library('cunit', required: true)
]

test_app = executable(test_app_name,
                      test_app_src,
                      dependencies: test_app_deps,
                      install: false)
subdir('suites')

testbot = run_target ('test_bot',
                      depends: [test_app],
                      command: [test_app])
test ('test_bot framewrok', test_app)
