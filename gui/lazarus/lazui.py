import os
import subprocess
from icons.handler import svg2png

def application_build (app, project, cpu, os, ws) :
    cmd = [app, '='.join(['--cpu', cpu]), '='.join(['--operating-system', os]), '='.join(['--ws', ws]), project]
    subprocess.check_call (cmd)

def lazui_build (app, project, cpu, os, ws, icons_in, icons_out) :
    application_build (app, project, cpu, os, ws)
    svg2png (icons_in, icons_out)
