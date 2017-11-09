# Load regular user profile, when logging in with rscreen
if [ -n "${REAL_HOME}" ] && [ -e "${REAL_HOME}/.profile" ]; then
    HOME="${REAL_HOME}" . "${REAL_HOME}/.profile"
fi

# Locale
export LC_ALL=en_US.UTF-8
export LC_TIME=sv_SE.UTF-8
export LC_COLLATE=C

# Path
for d in $HOME/bin $HOME/opt/*/bin; do
    if [ -d "$d" ]; then
        PATH="$d${PATH:+:$PATH}"
    fi
done

# Application preferences
export EDITOR=vim
export PAGER='less -r'

# Username for prod &c.
export ARBZRUSER=bnl

# Go
export GOPATH="$HOME/opt/go"

# Python
export PYTHONUSERBASE="$HOME/opt/python"

# Rust
export CARGO_HOME="$HOME/opt/cargo"
