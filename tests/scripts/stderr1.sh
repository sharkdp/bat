#!/bin/bash

## test for issue 2561

OUTPUT=$(mktemp)
BAT=bat
code=$($BAT /tmp 2> $OUTPUT; cat $OUTPUT | grep error; echo $?)

if [[ $code == 1 ]]; then
    echo "stderr test fsil"
    exit 1
fi

exit 0
