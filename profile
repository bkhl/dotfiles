# Locale
export LC_ALL=en_US.UTF-8
export LC_TIME=sv_SE.UTF-8
export LC_COLLATE=C

if command -v keychain > /dev/null; then
    eval $(keychain --quiet --quick --agents ssh --eval id_dsa)
fi

# Path
for d in $HOME/.bin $HOME/.local/bin /opt/local/sbin /opt/local/bin; do
    if [ -d $d ]; then
        PATH=$d:$PATH
    fi
done

# Preferences
export EDITOR=vim

# Other
export ARBZRUSER=bnl
