if !exists('b:ale_linters')
    let b:ale_linters = {}
endif

if !exists('b:ale_fixers')
    let b:ale_fixers = {}
endif

let b:ale_linters['python'] = ['pyls']
let b:ale_python_pyls_config = {
\   'pyls': {
\       'plugins': {
\           'pylint': { 'enabled': v:true },
\           'rope_completion': { 'enabled': v:true },
\       }
\   }
\}

let b:ale_fixers['python'] = ['isort', 'black']
let b:ale_fix_on_save = v:true
