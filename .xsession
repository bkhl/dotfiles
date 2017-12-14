#!/bin/sh

# Work-around to activate gnome-keyring SSH agent until
# I work out how to get lxsession to do it.
eval "$(gnome-keyring-daemon -s --components=pkcs11,secrets,ssh,gpg)"
export SSH_AUTH_SOCK

exec /usr/bin/lxsession -s Lubuntu -e LXDE
