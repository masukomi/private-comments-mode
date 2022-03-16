#!/bin/bash

# by mklement0 https://stackoverflow.com/a/29613573/5132008

# Define sample multi-line literal.
input=`cat`
replace="$input"
if [ ! -z "$3" ]; then
    replace=$(awk "BEGIN{IGNORECASE=1} /$3/,EOF { print \"   \" \$0 }" <<<"$input")
fi
FIND="$1"
FILE=$2

# Escape it for use as a Sed replacement string.
IFS= read -d '' -r < <(sed -e ':a' -e '$!{N;ba' -e '}' -e 's/[&/\]/\\&/g; s/\n/\\&/g' <<<"$replace")
replaceEscaped=${REPLY%$'\n'}

# If ok, outputs $replace as is.
gsed "/$FIND/c\\$replaceEscaped" $FILE
