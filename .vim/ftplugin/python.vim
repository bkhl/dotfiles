if !exists('b:ale_linters')
    let b:ale_linters = {}
endif

if !exists('b:ale_fixers')
    let b:ale_fixers = {}
endif

let b:ale_linters['python'] = ['pylint']
let b:ale_fixers['python'] = ['isort', 'black']

let g:ale_python_flake8_auto_pipenv = 1
