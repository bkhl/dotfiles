# Load regular user bashrc, when logging in with rscreen
if [ -n "${REAL_HOME}" -a -e "${REAL_HOME}/.bashrc" ]; then
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
if [ $BASH_VERSINFO -gt 4 ]; then
    shopt -s autocd
    shopt -s globstar
fi
GLOBIGNORE=.:..

# Check window size after each command
shopt -s checkwinsize

# Return code before prompt if non-zero
function _prompt_return_code {
    prompt_return_code=$?
    if [[ $prompt_return_code -eq 0 ]]; then
        prompt_return_code=
    else
        prompt_return_code="?:$prompt_return_code "
    fi
}
PROMPT_COMMAND=_prompt_return_code

PS1='${prompt_return_code}\u@\h \w\$ '

case $TERM in
    xterm*|*rxvt*)
    PS1="\[\033]0;\u@\h: \w\007\]${PS1}"
esac

# Aliases
alias du='du -h'
alias egrep='egrep --color=auto'
alias fgrep='fgrep --color=auto'
alias grep='grep --color=auto'
if [ "$(uname)" == Darwin ]; then
    if command -V gls > /dev/null; then
        alias ls='gls --color=auto'
    fi
elif ls --color > /dev/null; then
    alias ls='ls --color=auto'
fi

# Programmable completion
if ! shopt -oq posix; then
    if [[ -f /usr/share/bash-completion/bash_completion ]]; then
      . /usr/share/bash-completion/bash_completion
    elif [[ -f /etc/bash_completion ]]; then
      . /etc/bash_completion
    fi
fi

# vi: ts=4 et
