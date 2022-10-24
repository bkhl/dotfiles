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

# This allows you to bookmark your favorite places across the file system
# Define a variable containing a path and you will be able to cd into it regardless of the directory you're in
shopt -s cdable_vars


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

alias du='du -h'
alias egrep='egrep --color=auto'
alias fgrep='fgrep --color=auto'
alias grep='grep --color=auto'
alias ls='ls --color=auto --classify'
alias sudo='sudo '
alias emacs="emacsclient --alternate-editor="

if [[ -n $DISPLAY ]] && hash xclip 2> /dev/null; then
    alias clp='xclip -selection clipboard'
    alias pst='xclip -selection clipboard -o'
fi


####
# Prompt update command
#

_prompt_template="\u@\h:\w\$ "

__update_prompt() {
    local r=$?
    PS1="$_prompt_template"
    if [[ $r != 0 ]]; then
        PS1="\[\033[1;31m\]?:${r}\[\033[0m\] ${PS1}"
    fi
}

if (( ${VTE_VERSION:-0} < 3405 )) \
    || ! declare -f __vte_prompt_command > /dev/null; then
    __vte_prompt_command() { :; }
fi

__prompt_command() {
    __update_prompt
    __vte_prompt_command
    history -a
}

PROMPT_COMMAND=__prompt_command
