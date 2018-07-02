img_src_ext = 'svg'
img_src_dir = 'svg'
img_out_ext = 'png'
img_out_dir = 'out'

shell_file = 'gen_by_res.sh'
shell_template = ''.join([shell_file, '.in'])

img_resolutions = ['16' , '22'  , '24'  , '32',
                   '36' , '48'  , '64'  , '72',
                   '96' , '128' , '192' , '256',
                   '512', '1024']

# --- validation ImageMagic convert utilites ---
convert_app = find_program('convert', required: true)
if convert_app.found()
    convert_ver = run_command (convert_app, '-version').stdout().strip().split('\n')[0]
    message('Convert application version: @0@'.format(convert_ver))
else
    error('Convert application (ImageMagic part) NOT found')
endif


shell_data = configuration_data()
shell_data.set('working_dir', meson.current_source_dir())
shell_data.set('source_dir' , join_paths(meson.current_source_dir(), img_src_dir))
shell_data.set('target_dir' , join_paths(meson.current_source_dir(), img_out_dir))
shell_data.set('source_ext' , img_src_ext)
shell_data.set('target_ext' , img_out_ext)

configure_file(input: shell_template,
               output: shell_file,
               configuration: shell_data)

shell_file = join_paths(meson.current_build_dir(), shell_file)

targets_array = []

foreach res : img_resolutions
    if get_option('auto_generate_icons')
        conv_result = run_command(shell, shell_file, res)
    endif
    targets_array += run_target (''.join(['icons_', res]),
                                 command: [shell, shell_file, res])
endforeach
