#!/bin/sh -e

# https://www.unicode.org/L2/L1999/UnicodeData.html
#
# \1  = Code value
# \2  = Character name
# \3  = General category
# \4  = Canonical combining classes
# \5  = Bidirectional category
# \6  = Character decomposition mapping
# \7  = Decimal digit value
# \8  = Digit value
# \9  = Numeric value
# \10 = Mirrored
# \11 = Unicode 1.0 Name
# \12 = 10646 comment field
# \13 = Uppercase mapping
# \14 = Lowercase mapping
# \15 = Titlecase mapping

input="$(git rev-parse --show-toplevel)/configure/UnicodeData.txt"

for each in "$@"
do
  case "$each" in
    --property    ) sed -E 's/^([^;]*);([^;]*;){01}([^;]*).*$/case 0x\1: return   \3;/g' "$input"                                       ;;
    --digit-value ) sed -E 's/^([^;]*);([^;]*;){06}([^;]*).*$/case 0x\1: return   \3;/g' "$input" | grep -e 'case 0x.\+: return   .\+;' ;;
    --upcase      ) sed -E 's/^([^;]*);([^;]*;){11}([^;]*).*$/case 0x\1: return 0x\3;/g' "$input" | grep -e 'case 0x.\+: return 0x.\+;' ;;
    --downcase    ) sed -E 's/^([^;]*);([^;]*;){12}([^;]*).*$/case 0x\1: return 0x\3;/g' "$input" | grep -e 'case 0x.\+: return 0x.\+;' ;;
  esac
done
