# --- simple test suites ---

py3_rt = import('python3').find_python()
glob_cmd = [py3_rt, '-c', 'from sys import argv; print(";".join(__import__("glob").glob(argv[1])))']

suites_simple = run_command( glob_cmd + ['*.c']).stdout().strip().split(';')

foreach suite : suites_simple
    lib_name = suite.split('.')[0]
    lib_suite = shared_library (lib_name, [test_app_utils, suite],
                                dependencies: test_app_deps,
                                include_directories: libs_inc,
                                link_with: libucma,
                                install: false)
endforeach
