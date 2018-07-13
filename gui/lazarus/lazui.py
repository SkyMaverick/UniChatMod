import os
import subprocess
import psutil
from convert import conv_p2p

icons_resolutions = [16, 32, 48, 64, 128, 256]
icons_src_ext = '.svg'
icons_out_ext = '.png'

def svg2png (in_path, out_path) :
    src_path = os.path.realpath (os.path.normpath (in_path))
    out_path = os.path.realpath (os.path.normpath (out_path))

    for res in icons_resolutions :
        conv_p2p (src_path, out_path, icons_src_ext, icons_out_ext, res)

def application_build (app, project, pkgs, mode, cpu, os, ws) :
    build_arch = [  '='.join ( ['--build-mode'          , mode] ), 
                    '='.join ( ['--cpu'                 , cpu] ),
                    '='.join ( ['--operating-system'    , os] ),
                    '='.join ( ['--ws'                  , ws] ),
                    '='.join ( ['--max-process-count'   , str (psutil.cpu_count())] )
    ]
# --- create depends packages ---
    for pkg in pkgs :
        pkg_cmd = [app] + build_arch + [pkg]
        subprocess.check_call (pkg_cmd)

# --- create app ---
    cmd = [app] + build_arch + [project]
    subprocess.check_call (cmd)

def lazui_build (app, project, pkgs, mode, cpu, os, ws, icons_in, icons_out) :
    application_build (app, project, pkgs, mode, cpu, os, ws)
    svg2png (icons_in, icons_out)
