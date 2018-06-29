img_resolutions = ['16', '22', '24', '32',
                   '36', '48', '64', '72',
                   '96', '128','192','256',
                   '512','1024','2048']

shell_file = join_paths(meson.current_source_dir(), 'gen_by_res.sh')
foreach res : img_resolutions
   conv_result = run_command(shell, shell_file, res) 
endforeach
