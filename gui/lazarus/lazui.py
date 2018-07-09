import os
import subprocess
import psutil
from icons.handler import svg2png

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
