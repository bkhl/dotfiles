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
alias micro="$(which micro)"
alias m=micro
alias sudo='sudo '

# Prompt
PS1='$(r=$?; if [[ $r != 0 ]]; then echo "?:$r "; fi)\u@\h \w\$ '

# Set window titles
case "${TERM}" in
    xterm*|*rxvt*)
        # Get from https://github.com/rcaloras/bash-preexec
        if [[ -e "${HOME}/.bash/bash-preexec.sh" ]] && command -V xtitle > /dev/null 3>&1; then
            source "${HOME}/.bash/bash-preexec.sh"
            if [[ ${UID} = 0 ]]; then
                PROMPT_CHARACTER='#'
            else
                PROMPT_CHARACTER='$'
            fi
            get_title() {
                echo "${USER}@${HOSTNAME} ${PWD/#${HOME}/\~}${PROMPT_CHARACTER}";
            }
            precmd() {
                xtitle -t "$(get_title)"
            }
            preexec() {
                xtitle -t "$(get_title) $1"
            }
        else
            PS1="\[\033]0;\u@\h \w\$\007\]${PS1}"
        fi
esac

# vi: ts=4 et
