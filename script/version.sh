#!/bin/sh

root="$(git rev-parse --show-toplevel)"

major=0
major_offset=0

minor=2
minor_offset=1000

revision="$(git rev-list --count --all)"
patch="$((revision - major_offset - minor_offset))"

printf '%d.%d.%d\n' "$major" "$minor" "$patch" | tee "$root/VERSION"
