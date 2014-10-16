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
