#!/bin/sh

FILTER_FILE="$(mktemp --tmpdir= afuse_filter_file.XXXXXXXX)"
POPULATE_FILE="$(mktemp --tmpdir= afuse_remote_file.XXXXXXXX)"
MOUNT_POINT='/remote'

cat << EOF > "${FILTER_FILE}"
autorun.inf
EOF

cat << EOF > "${POPULATE_FILE}"
ardome@core01.mum.zeel
spirit
mamstorage.dayjob.internal.example
EOF

afuse \
    -o filter_file="${FILTER_FILE}" \
    -o mount_template="sshfs -o transform_symlinks -o reconnect -o workaround=rename:nodelaysrv:buflimit -o kernel_cache %r:/ %m" \
    -o unmount_template="fusermount -u -z %m" \
    -o populate_root_command="cat ${POPULATE_FILE}" \
    "${MOUNT_POINT}"

rm "${FILTER_FILE}" "${POPULATE_FILE}"
