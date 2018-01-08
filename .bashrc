# Load regular user bashrc, when logging in with rscreen
if [[ -n "${REAL_HOME}" ]] && [[ -e "${REAL_HOME}/.bashrc" ]]; then
    HOME="${REAL_HOME}" source "${REAL_HOME}/.bashrc"
fi

# History
HISTCONTROL=erasedups:ignorespace
HISTFILESIZE=16384
HISTSIZE=8192
shopt -s histappend
shopt -s cmdhist # Save multi-line command as single history entry

# Globbing
shopt -s extglob
if [[ $BASH_VERSINFO -ge 4 ]]; then
    shopt -s autocd
    shopt -s globstar
fi
GLOBIGNORE=.:..

# Check window size after each command
shopt -s checkwinsize

# Programmable completion
if ! shopt -oq posix; then
    if [[ -f /usr/share/bash-completion/bash_completion ]]; then
      . /usr/share/bash-completion/bash_completion
    elif [[ -f /etc/bash_completion ]]; then
      . /etc/bash_completion
    fi
fi

# Aliases
alias du='du -h'
alias egrep='egrep --color=auto'
alias fgrep='fgrep --color=auto'
alias grep='grep --color=auto'
alias ls='ls --color=auto'
alias sudo='sudo '

if [[ "$TERM" =~ ^xterm(-256color)?$ ]] && command -V gvim > /dev/null 2>&1; then
    function vim {
        if [[ -z "$(gvim --serverlist)" ]]; then
            gvim "$@"
        else
            gvim --remote "$@"
        fi
    }
fi

# Colour scheme
if [[ "${TERM}" =~ ^xterm(-256color)?$ ]]; then
    BASE16_SHELL="${HOME}/.config/base16-shell"
    eval "$("${BASE16_SHELL}"/profile_helper.sh)"
fi
if alias base16_bright > /dev/null 2>&1; then
    base16_bright
fi

# Function for getting current git branch, to show in prompt
if command -V git > /dev/null 2>&1; then
    _get_git_branch() {
        branch=$(git branch 2> /dev/null | sed '/^[^*]/d; s/* \(.*\)/\1/')
        if [[ $branch != master ]]; then
            echo "(${branch}) "
        fi
    }
else
    _get_git_branch() {
        :
    }
fi

# Prompt
PS1='$(r=$?; if [[ $r != 0 ]]; then echo "?:$r "; fi)$(_get_git_branch)\u@\h \w\$ '

# vi: ts=4 et
