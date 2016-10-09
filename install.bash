#!/bin/bash

CONFIG_REPO="${HOME}/Config"

TARGETS=(
    bash_profile
    bashrc
    gitconfig
    gvimrc
    hgrc
    i3
    inputrc
    profile
    screenrc
    vim
    vimrc
    Xresources
) 

for target in ${TARGETS[@]}; do 
    absolute_target="${CONFIG_REPO}/${target}"
    link_name="${HOME}/.${target}"
    if [ -e "${link_name}" -a ! -L "${link_name}" ]; then
        echo "'${link_name}' exists and is not a symlink, not overwriting."
    else
        ln -frsTv "${absolute_target}" "${link_name}"
    fi
done
