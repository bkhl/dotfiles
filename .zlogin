if [[ "${TERM}" == 'screen' && "${PWD}" != "${HOME}" ]]; then
    cd "${HOME}"
fi
