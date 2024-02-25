_default:
    just --list

build_iosevka:
    #!/bin/bash

    set -xeuo pipefail

    font_dir="{{ justfile_directory () }}/.local/share/fonts/Iosevka"
    build_dir="$(mktemp -d -t iosevka.XXXXXXXX)"

    for plan in ttf::iosevka-bkhl-{sans,serif}-{normal,fixed}; do
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
