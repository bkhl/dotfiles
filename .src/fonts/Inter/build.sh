#!/bin/bash

set -xeuo pipefail

curl -sSLO 'https://github.com/rsms/inter/releases/download/v4.0/Inter-4.0.zip'
sha256sum -c - <<HERE
ff970a5d4561a04f102a7cb781adbd6ac4e9b6c460914c7a101f15acb7f7d1a4 Inter-4.0.zip
HERE

rm -rf src
mkdir src
pushd src
unzip ../Inter-4.0.zip
popd

for suffix in "" "-Italic"; do
/opt/opentype-feature-freezer/bin/pyftfeatfreeze \
    --features 'ss04' \
    --replacenames 'Inter Variable/Inter BKHL Variable' \
    "src/InterVariable${suffix}.ttf" "Inter BKHL Variable${suffix}.ttf"
done
