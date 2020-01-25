if !exists('b:ale_fixers')
    let b:ale_fixers = {}
endif

let b:ale_fixers['d'] = ['dfmt']

let b:ale_fix_on_save = 1
