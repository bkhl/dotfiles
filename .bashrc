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
alias e="emacsclient --no-wait"


####
# Prompt
#

if (( ${VTE_VERSION:-0} < 3405 )) \
    || ! declare -f __vte_prompt_command > /dev/null; then
    __vte_prompt_command() { :; }
fi

function __git_prompt_command() {
    unset __git_prompt_name __git_prompt_colour __git_prompt_style

    local root="$(git rev-parse --show-toplevel 2> /dev/null)"
    [[ -z $root || $root == $HOME ]] && return

    __git_prompt_name="$(git branch --show-current)"
    if [[ -n $__git_prompt_name ]]; then
        __git_prompt_color='32'
    else
        __git_prompt_name="$(git tag --points-at HEAD | head -1)"
        if [[ -n $__git_prompt_name ]]; then
            __git_prompt_color='36'
        else
            __git_prompt_name="$(git rev-parse --short HEAD)"
            if [[ -n $__git_prompt_name ]]; then
                __git_prompt_color='33'
            else
                return
            fi
        fi
    fi

    if git diff --quiet; then
        __git_prompt_style='0'
    else
        __git_prompt_style='3'
    fi
}

__prompt_command() {
    __git_prompt_command
    __vte_prompt_command
    history -a
}

PROMPT_COMMAND=__prompt_command

__ps1_rc_map=([0]=)
__ps1_rc='${__ps1_rc_map[$?]-\[\e[0;31m\]?:$?\[\e[0m\] }'
__ps1_host='\u@\h'
__ps1_cwd='\[\e[0;34m\]\w\[\e[0m\]'
__ps1_git='${__git_prompt_name:+ \[\e[${__git_prompt_style};${__git_prompt_color}m\]${__git_prompt_name}\[\e[0m\]}'

PS1="[${__ps1_rc}${__ps1_host} ${__ps1_cwd}${__ps1_git}]\$ "
