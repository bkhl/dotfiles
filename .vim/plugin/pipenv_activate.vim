" Try to activate virtualenv created by Pipenv.
"
" If called from .lvimrc, use the directory of that .lvimrc, otherwise use
" current working directory.
"
" The virtual environment is activated using
" https://github.com/plytophogy/vim-virtualenv

function! PipenvActivate()
py3 << EOF
import os
import subprocess

try:
    os.chdir(vim.eval("g:localvimrc_script_dir"))
except:
    pass

path = subprocess.check_output(["pipenv", "--venv"]).splitlines()[0]
_, name = os.path.split(path)
vim.command(f"VirtualEnvActivate {name.decode('utf8')}")
EOF
endfunction
