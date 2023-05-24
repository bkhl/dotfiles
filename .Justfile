default:
    just --list

IOSEVKA_VERSION := "23.0.0"
IOSEVKA_BUILD_IMAGE := "docker.io/avivace/iosevka-build"

build_iosevka:
    #!/bin/bash

    set -xeuo pipefail

    font_dir="{{ justfile_directory () }}/.local/share/fonts/Iosevka"
    build_dir="$(mktemp -d -t iosevka.XXXXXXXX)"

    podman run --rm \
        -v "${build_dir}:/build:z" \
        -v "${font_dir}/private-build-plans.toml:/build/private-build-plans.toml:z" \
        -e FONT_VERSION="{{ IOSEVKA_VERSION  }}" \
        '{{ IOSEVKA_BUILD_IMAGE }}' \
        contents::iosevka-bkhl-default \
        contents::iosevka-bkhl-fixed

    cp -v "${build_dir}"/dist/iosevka-bkhl-*/ttf/*.ttf "${font_dir}/"

    echo "{{ IOSEVKA_VERSION }}" > "${font_dir}/FONT_VERSION"

    rm -rfv "${build_dir}"
