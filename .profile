# Locale
if LC_ALL=C locale -a | grep -q '^en_US.utf8$'; then
    export LANG=en_US.utf8
elif LC_ALL=C locale -a | grep -q '^C.utf8$'; then
    export LANG=C.utf8
else
    export LANG=C
fi

export LANGUAGE="$LANG"
export LC_COLLATE="$LANG"
export LC_CTYPE="$LANG"
export LC_IDENTIFICATION="$LANG"
export LC_NUMERIC="$LANG"
export LC_MESSAGES="$LANG"

export LC_ALL=

if locale -a | grep -q '^sv_SE.utf8$'; then
    export LC_ADDRESS=sv_SE.utf8
    export LC_MEASUREMENT=sv_SE.utf8
    export LC_MONETARY=sv_SE.utf8
    export LC_NAME=sv_SE.utf8
    export LC_PAPER=sv_SE.utf8
    export LC_TELEPHONE=sv_SE.utf8
    export LC_TIME=sv_SE.utf8
else
    export LC_ADDRESS="$LANG"
    export LC_MEASUREMENT="$LANG"
    export LC_MONETARY="$LANG"
    export LC_NAME="$LANG"
    export LC_PAPER="$LANG"
    export LC_TELEPHONE="$LANG"
    export LC_TIME="$LANG"
fi


# Path
for d in $HOME/.local/bin; do
    if [ -d "$d" ]; then
        PATH="$d${PATH:+:$PATH}"
    fi
done

# Application preferences
export EDITOR=vim
export PAGER='less -R'

# Python
export PIPENV_VENV_IN_PROJECT=1
