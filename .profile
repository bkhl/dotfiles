# Load regular user profile, when logging in with rscreen
if [ -n "${REAL_HOME}" ] && [ -e "${REAL_HOME}/.profile" ]; then
    # shellcheck source=/dev/null
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
for d in $HOME/.local/bin $HOME/opt/*/bin $HOME/opt/asdf/shims; do
    if [ -d "$d" ]; then
        PATH="$d${PATH:+:$PATH}"
    fi
done

# Application preferences
export EDITOR=vim
export PAGER='less -R'

# Username for prod &c.
export ARBZRUSER=bnl

# asdf
export ASDF_DIR="$HOME/opt/asdf"
export ASDF_DATA_DIR="$ASDF_DIR"

# Go
export GOPATH="$HOME/opt/go"

# Java
export SDKMAN_DIR="$HOME/opt/sdkman"

# Python
export PYTHONUSERBASE="$HOME/opt/python"
export PIPENV_IGNORE_VIRTUALENVS=2 # Don't warn when falling back to already activated virtual environment.

# Rust
export CARGO_HOME="$HOME/opt/cargo"
