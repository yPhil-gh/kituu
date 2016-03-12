#!/usr/bin/env bash

set -o errexit
set -o pipefail
set -o nounset
# set -o xtrace

# Set magic variables for current file & dir
__dir="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
__file="${__dir}/$(basename "${BASH_SOURCE[0]}")"
__base="$(basename ${__file} .sh)"
__root="$(cd "$(dirname "${__dir}")" && pwd)" # <-- change this as it depends on your app

arg1="${1:-}"

for plugin in $(lv2ls); do
    NAME=$(lv2info "$plugin" | grep 'Name:' | grep -v http | awk '{print $0;exit}')
    CLASS=$(lv2info "$plugin" | grep 'Class:' | grep -v http | awk '{print $2;exit}')
    [[ "${CLASS}" = "Instrument" ]] && tput setaf 1 || tput setaf 3
    printf "%-20s" ${CLASS}
    tput setaf 2 ; echo ${NAME#*:}
done
