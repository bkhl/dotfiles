# Load regular user profile, when logging in with rscreen
if [ -n "${REAL_HOME}" ] && [ -e "${REAL_HOME}/.profile" ]; then
    HOME="${REAL_HOME}" . "${REAL_HOME}/.profile"
fi

# Locale
export LANG=en_US.UTF-8
export LANGUAGE=en
export LC_ADDRESS=C
export LC_COLLATE=C
export LC_IDENTIFICATION=C
export LC_MEASUREMENT=C
export LC_MONETARY=C
export LC_NAME=C
export LC_NUMERIC=C
export LC_PAPER=C
export LC_TELEPHONE=C
export LC_TIME=sv_SE.UTF-8

# Disable accessability bridge
export NO_AT_BRIDGE=1

# Path
for d in $HOME/bin $HOME/opt/*/bin $HOME/opt/asdf/shims; do
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

# Java
export SDKMAN_DIR="$HOME/opt/sdkman"

# Python
export PYTHONUSERBASE="$HOME/opt/python"

# Rust
export CARGO_HOME="$HOME/opt/cargo"
