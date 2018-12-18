# Load regular user bashrc, when logging in with rscreen
if [[ -n "${REAL_HOME}" ]] && [[ -e "${REAL_HOME}/.bashrc" ]]; then
    HOME="${REAL_HOME}" source "${REAL_HOME}/.bashrc"
fi

# Prevent file overwrite on stdout redirection
# Use `>|` to force redirection to an existing file
set -o noclobber

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

# Case-insensitive globbing (used in pathname expansion)
shopt -s nocaseglob;


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
CDPATH="."

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
alias sudo='sudo '
alias fd='fd --no-ignore-vcs'

if hash exa 2> /dev/null; then
    alias ls='exa --classify --git'
else
    alias ls='ls --color=auto --classify'
fi

if [[ -n $DISPLAY ]] && hash xsel 2> /dev/null; then
    alias clip='xsel --clipboard'
fi


####
# Prompt update command
#

# Prompt command
_update_prompt() {
    local r=$?
    PS1='\u@\h:\w\$ '
    if [[ $r != 0 ]]; then
        PS1="?:${r} ${PS1}"
    fi
}

_prompt_command() {
    history -a
    _update_prompt
}

PROMPT_COMMAND=_prompt_command


####
# Package manager loading.
#

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
