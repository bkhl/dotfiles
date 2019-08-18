if !exists('b:ale_fixers')
    let b:ale_fixers = {}
endif

let b:ale_fixers['d'] = ['dfmt']

let b:ale_d_dfmt_executable = 'dub'
let b:ale_d_dfmt_options = 'run --quiet dfmt --'
