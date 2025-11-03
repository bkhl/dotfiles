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

unset LC_ALL

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
for d in $HOME/.local/bin $HOME/.local/opt/*/bin; do
    if [ -d "$d" ]; then
        PATH="$d${PATH:+:$PATH}"
    fi
done

# Library path
for d in $HOME/.local/opt/*/lib; do
    if [ -d "$d" ]; then
        LD_LIBRARY_PATH="$d${LD_LIBRARY_PATH:+:$LD_LIBRARY_PATH}"
    fi
done

if [ -n "$LD_LIBRARY_PATH" ]; then
    export LD_LIBRARY_PATH
fi

# Enable using emacsclient as editor of other commands. Setting ALTERNATE_EDITOR
# like this makes emacsclient start a server in the background and try to
# reconnect, if it can't find one.
export EDITOR="emacsclient --quiet --tty --create-frame"
export ALTERNATE_EDITOR=""

# Use less with -R (show ANSI colour)
export PAGER='less -R'
