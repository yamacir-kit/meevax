#!/bin/sh

"true"; exec > "$(dirname $0)/$(basename $0 .ss).a"
"true"; echo "a"
"true"; exec "$@" -e "(begin $(cat $0))" 1> "$(dirname $0)/$(basename $0 .ss).b"

(quote a)

