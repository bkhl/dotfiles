#!/bin/bash

set -xeuo pipefail

curl -sSLO 'https://github.com/rsms/inter/releases/download/v4.1/Inter-4.1.zip'
sha256sum -c - <<HERE
9883fdd4a49d4fb66bd8177ba6625ef9a64aa45899767dde3d36aa425756b11e Inter-4.1.zip
HERE

rm -rf src
mkdir src
pushd src
unzip ../Inter-4.1.zip
popd

for suffix in "" "-Italic"; do
/opt/opentype-feature-freezer/bin/pyftfeatfreeze \
    --features 'ss04' \
    --replacenames 'Inter Variable/Inter BKHL Variable' \
    "src/InterVariable${suffix}.ttf" "Inter BKHL Variable${suffix}.ttf"
done
