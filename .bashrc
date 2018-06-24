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
alias ls='ls --color=auto --classify'
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

# Prompt
_update_prompt() {
    local r=$?
    PS1='\u@\h:\w\$ '
    if [[ $r != 0 ]]; then
        PS1="?:${r} ${PS1}"
    fi
}
PROMPT_COMMAND=_update_prompt

# Window title
_update_title () {
    if [ "$BASH_COMMAND" == '_update_prompt' ]; then
        local t="$USER@$HOSTNAME:$(dirs +0)"
    elif [ "$1" ]; then
        local t="$*"
    else
        local t="$BASH_COMMAND"
    fi
    printf "\e]0;%s\007" "$t"
}
trap _update_title DEBUG

_foreground() {
    case "$1" in
        [0-9]*([0-9]))
            local p=$(jobs -l | grep -e "^\[$1\]" | cut -d' ' -f2);;
        +|-)
            local p=$(jobs -l | grep -e "^\[[[:digit:]]\+\]$1" | cut -d' ' -f2);;
        *)
            local p=$(jobs -l | grep -e "^\[[[:digit:]]\+\]+" | cut -d' ' -f2);;
    esac
    _update_title $(tr '\000' ' ' < /proc/$p/cmdline)
    command fg $1
}
alias fg=_foreground

# Java
if [[ -e "${HOME}/opt/sdkman/bin/sdkman-init.sh" ]]; then
    source "${HOME}/opt/sdkman/bin/sdkman-init.sh"
fi

# vi: ts=4 et
