# Source global definitions
if [ -f /etc/bashrc ]; then
    source /etc/bashrc
fi

# Update window size after every command.
shopt -s checkwinsize

# Automatically trim long paths in the prompt.
if ((BASH_VERSINFO[0] >= 4)); then
    PROMPT_DIRTRIM=2
fi

# Turn on recursive globbing (enables ** to recurse all directories)
if ((BASH_VERSINFO[0] >= 4)); then
    shopt -s globstar
fi


####
# History
#

# Append to the history file, don't overwrite it
shopt -s histappend

# Save multi-line commands as one command
shopt -s cmdhist

# Huge history. Doesn't appear to slow things down, so why not?
HISTSIZE=524288
HISTFILESIZE=131072

# Avoid duplicate entries
HISTCONTROL=erasedups:ignoreboth

# Don't record some commands
HISTIGNORE="&:[ ]*:exit:ls:bg:fg:history:clear"

# Readable time format
HISTTIMEFORMAT='%F %T '


####
# Directory navigation
#

if ((BASH_VERSINFO[0] >= 4)); then
    # Prepend cd to directory names automatically
    shopt -s autocd

    # Correct spelling errors during tab-completion
    shopt -s dirspell

    # Correct spelling errors in arguments supplied to cd
    shopt -s cdspell
fi

# This defines where cd looks for targets
# Add the directories you want to have fast access to, separated by colon
CDPATH=".:~"


####
# Globbing
#

# Allow expanded glob syntax
shopt -s extglob

# Exclude "." and ".." from glob expansion
GLOBIGNORE=.:..


####
# Programmable completion
#

if ! shopt -oq posix; then
    if [[ -f /usr/share/bash-completion/bash_completion ]]; then
      . /usr/share/bash-completion/bash_completion
    elif [[ -f /etc/bash_completion ]]; then
      . /etc/bash_completion
    fi
fi


####
# Aliases
#

alias du='du --human-readable'
alias grep='grep --color=auto'
alias ls='ls --color=auto --classify --group-directories-first'
alias ll='ls -l --color=auto --classify --group-directories-first'
alias l.='ls -d --color=auto --classify --group-directories-first .*'
alias ip='ip --color=auto'
alias sudo='sudo '
alias emacs="emacsclient --alternate-editor= --no-wait"


####
# Prompt
#

function _ps1_git_status() {
    local root="$(git rev-parse --show-toplevel 2> /dev/null)"
    [[ -z $root || $root == $HOME ]] && return

    local name="$(git branch --show-current)"
    [[ -z $name ]] && name="$(git tag --points-at HEAD | head -1)"
    [[ -z $name ]] && name="$(git rev-parse --short HEAD)"

    local status=""
    if ! git diff --quiet; then
        status="*"
    fi

    printf " %s%s" "$name" "$status"
}

_ps1_aux=([0]=)
PS1='[${_ps1_aux[$?]-\[\e[0;31m\]?:$?\[\e[0m\] }\[\e[2;33m\]\u@\h\[\e[0m\] \[\e[0;34m\]\w\[\e[0m\]\[\e[2;32m\]$(_ps1_git_status)\[\e[0m\]]\$ '
