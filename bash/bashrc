# History
HISTCONTROL=erasedups:ignorespace
HISTFILESIZE=16384
HISTSIZE=8192
shopt -s histappend
shopt -s cmdhist # Save multi-line command as single history entry

shopt -s autocd
shopt -s checkwinsize

# Globbing
shopt -s extglob
shopt -s globstar

# Return code before prompt if non-zero

# Prompt and window title
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

if [ -n "$DISPLAY" ]; then
    command -v gvim > /dev/null && alias gv='gvim --remote-tab'
    
    if command -v ssh > /dev/null; then
        function xssh {
            (xterm -e ssh "$@" &)
        }
    fi
     
    if command -v mosh > /dev/null; then
        function xmosh {
            (xterm -e mosh "$@" &)
        }
    fi
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
