# Load regular user profile, when logging in with rscreen
if [ -n "${REAL_HOME}" -a -e "${REAL_HOME}/.profile" ]; then
    HOME="${REAL_HOME}" source "${REAL_HOME}/.profile"
fi

# Locale
export LC_ALL=en_US.UTF-8
export LC_TIME=sv_SE.UTF-8
export LC_COLLATE=C

# Path
for d in $HOME/bin $HOME/.local/bin $HOME/.npm/bin /opt/local/sbin /opt/local/bin; do
    if [ -d $d ]; then
        PATH=$d:$PATH
    fi
done

# Python virtual environment
if [ -d $HOME/venv ]; then
    echo found it
    VIRTUAL_ENV_DISABLE_PROMPT=1 . $HOME/venv/bin/activate
fi

# Application preferences
export EDITOR=vim
export PAGER='less -r'

# Username for prod &c.
export ARBZRUSER=bnl
