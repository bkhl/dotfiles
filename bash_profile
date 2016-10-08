# Load regular user bash_profile, when logging in with rscreen
if [ -n "${REAL_HOME}" -a -e "${REAL_HOME}/.bash_profile" ]; then
    HOME="${REAL_HOME}" source "${REAL_HOME}/.bash_profile"
fi

# Read ~/.profile
if [ -n $HOME/.profile ]; then
    source $HOME/.profile
fi

# Always read ~/.bashrc for interactive shells
if [ -n "$BASH" ]; then
    case $- in
        *i*) source $HOME/.bashrc ;;
          *) return ;;
    esac
fi
