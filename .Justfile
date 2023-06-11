_default:
    just --list

INTERFACE_FONT := "Inter 11"
DOCUMENT_FONT := "Inter 11"
MONOSPACE_FONT := "Iosevka BKHL Fixed 13"

INPUT_SOURCES := "[('xkb', 'us+altgr-intl'), ('xkb', 'th')]"

# Apply desktop configuration
configure:
    # Fonts
    dconf write /org/gnome/desktop/interface/font-name "'{{ INTERFACE_FONT }}'"
    dconf write /org/gnome/desktop/interface/document-font-name "'{{ DOCUMENT_FONT }}'"
    dconf write /org/gnome/desktop/interface/monospace-font-name "'{{ MONOSPACE_FONT }}'"
    dconf write /org/gnome/desktop/wm/preferences/titlebar-font "'{{ INTERFACE_FONT }}'"

    # Disable activity hot corner
    dconf write /org/gnome/desktop/interface/enable-hot-corners 'false'

    # Add some menu bar buttons
    dconf write /org/gnome/desktop/wm/preferences/button-layout "'appmenu:minimize,maximize,close'"

    # Keyboard settings
    dconf write /org/gnome/desktop/input-sources/sources "{{ INPUT_SOURCES }}"
    dconf write /org/gnome/desktop/input-sources/mru-sources "{{ INPUT_SOURCES }}"
    dconf write /org/gnome/desktop/input-sources/per-window 'false'
    dconf write /org/gnome/desktop/input-sources/xkb-options "['lv3:ralt_switch', 'compose:caps']"

    # Keyboard shortcut to launch terminal
    dconf write /org/gnome/settings-daemon/plugins/media-keys/custom-keybindings/custom0/name "'Terminal'"
    dconf write /org/gnome/settings-daemon/plugins/media-keys/custom-keybindings/custom0/binding "'<Super>t'"
    dconf write /org/gnome/settings-daemon/plugins/media-keys/custom-keybindings/custom0/command "'/usr/bin/gnome-terminal'"

IOSEVKA_VERSION := "24.1.2"
IOSEVKA_BUILD_IMAGE := "docker.io/avivace/iosevka-build"

build_iosevka:
    #!/bin/bash

    set -xeuo pipefail

    font_dir="{{ justfile_directory () }}/.local/share/fonts/Iosevka"
    build_dir="$(mktemp -d -t iosevka.XXXXXXXX)"

    for plan in ttf::iosevka-bkhl-{default,fixed}; do
        podman run --rm -t \
            -v "${build_dir}:/build:z" \
            -v "${font_dir}/private-build-plans.toml:/build/private-build-plans.toml:z" \
            -e FONT_VERSION="{{ IOSEVKA_VERSION  }}" \
            '{{ IOSEVKA_BUILD_IMAGE }}' \
            "${plan}"
    done

    cp -v "${build_dir}"/dist/iosevka-bkhl-*/ttf/*.ttf "${font_dir}/"

    echo "{{ IOSEVKA_VERSION }}" > "${font_dir}/FONT_VERSION"

    rm -rfv "${build_dir}"
