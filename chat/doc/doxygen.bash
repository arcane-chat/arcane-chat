#!/usr/bin/env bash
# -*- mode: sh; sh-shell: bash; coding: utf-8; -*-

usage () {
    echo "Usage: doxygen.bash EXECUTABLE ARGS..."
    echo ""
    echo "FIXME: doc"
    exit 1
}

{ (( $# >= 1 )) && [[ -x "${1}" ]]; } || usage

DOXYGEN_EXECUTABLE="${1}"
shift 1
DOXYGEN_ARGS="${*}"

run-doxygen () {
    eval "${DOXYGEN_EXECUTABLE} ${DOXYGEN_ARGS}"
}

# parse-doxygen () {
#     SEARCHING_REGEX='^Searching for ([[:alpha:][:space:]]+)(\.\.\.)?$'
#     SEARCHING_REPLACE='%%%(searching "\1")'
#     PREPROCESSING_REGEX='^Preprocessing (.+)[.]{3}$'
#     PREPROCESSING_REPLACE='%%%(preprocessing "\1")'
#     PARSING_REGEX='^Parsing file (.+)[.]{3}$'
#     PARSING_REPLACE='%%%(parsing "\1")'
#     cat - \
#         | sed -r "s|${SEARCHING_REGEX}|${SEARCHING_REPLACE}|g"         \
#         | sed -r "s|${PREPROCESSING_REGEX}|${PREPROCESSING_REPLACE}|g" \
#         | sed -r "s|${PARSING_REGEX}|${PARSING_REPLACE}|g"             \
#         | sed -r 's|%%%|\n%%%|g' \
#         | grep -v '^$' \
#         | grep -v '^%%%' \
#         | cat -
# }

# run-doxygen \
#     | parse-doxygen \
#     | guile -s doxygen


run-doxygen |& cat \
    | sed 's/\\n/\n/g' \
    | sed "s/\\\\e/$(echo -e '\e')/g"
