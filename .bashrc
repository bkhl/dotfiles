# Load regular user bashrc, when logging in with rscreen
if [[ -n "${REAL_HOME}" ]] && [[ -e "${REAL_HOME}/.bashrc" ]]; then
    HOME="${REAL_HOME}" source "${REAL_HOME}/.bashrc"
fi

# History
HISTCONTROL=erasedups:ignoreboth
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
alias sudo='sudo '
alias fd='fd --no-ignore-vcs'

if hash exa 2> /dev/null; then
    alias ls='exa --classify --git'
else
    alias ls='ls --color=auto --classify'
fi

# Prompt command
_update_prompt() {
    local r=$?
    PS1='\u@\h:\w\$ '
    if [[ $r != 0 ]]; then
        PS1="?:${r} ${PS1}"
    fi
}

_update_history() {
    history -n
    history -w
    history -c
    history -r
}

_prompt_command() {
    _update_prompt
    _update_history
}

PROMPT_COMMAND=_prompt_command

# asdf
if [[ -e "${ASDF_DIR}/asdf.sh" && -e "${ASDF_DIR}/completions/asdf.bash" ]]; then
    source "${ASDF_DIR}/asdf.sh"
    source "${ASDF_DIR}/completions/asdf.bash"
fi

# Java
if [[ -e "${HOME}/opt/sdkman/bin/sdkman-init.sh" ]]; then
    source "${HOME}/opt/sdkman/bin/sdkman-init.sh"
fi

# vi: ts=4 et
