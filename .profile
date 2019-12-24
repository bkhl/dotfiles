# Locale
export LANG=C.utf8
export LANGUAGE=en

if locale -a | grep -q '^sv_SE.utf8$'; then
    export LC_ADDRESS=sv_SE.utf8
    export LC_MEASUREMENT=sv_SE.utf8
    export LC_MONETARY=sv_SE.utf8
    export LC_NAME=sv_SE.utf8
    export LC_PAPER=sv_SE.utf8
    export LC_TELEPHONE=sv_SE.utf8
    export LC_TIME=sv_SE.utf8
else
    export LC_ADDRESS=C.utf8
    export LC_MEASUREMENT=C.utf8
    export LC_MONETARY=C.utf8
    export LC_NAME=C.utf8
    export LC_PAPER=C.utf8
    export LC_TELEPHONE=C.utf8
    export LC_TIME=C.utf8
fi

export LC_COLLATE=C.utf8
export LC_IDENTIFICATION=C.utf8
export LC_NUMERIC=C.utf8

# Path
for d in $HOME/.local/bin; do
    if [ -d "$d" ]; then
        PATH="$d${PATH:+:$PATH}"
    fi
done

# Application preferences
export EDITOR=vim
export PAGER='less -R'
