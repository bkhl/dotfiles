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

PS1='[\[\033[1;31m\]$(r=$?; (( r == 0 )) || printf "?:%s " $r)\[\033[0m\]\[\e[1m\]\u@\h \w\[\e[0m\]]\$ '
