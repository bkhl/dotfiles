if !exists('b:ale_linters')
    let b:ale_linters = {}
endif

if !exists('b:ale_fixers')
    let b:ale_fixers = {}
endif

let b:ale_linters['python'] = ['pylint']
let b:ale_fixers['python'] = ['isort', 'black']

let b:ale_fix_on_save = 1
let b:ale_python_auto_pipenv = 1
