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

unicode_data='^([^;]*);([^;]*);([^;]*);([^;]*);([^;]*);([^;]*);([^;]*);([^;]*);([^;]*);([^;]*);([^;]*);([^;]*);([^;]*);([^;]*);(.*)$'

substitute()
{
  sed -E "s/$unicode_data/$1/g" "$(git rev-parse --show-toplevel)/configure/UnicodeData.txt"
}

for each in "$@"
do
  case "$each" in
    --digit-value ) substitute '{ 0x\1, make_number("\9") },' | grep -e '{ .\+, make_number(".\+") },' ;;
    --property    ) substitute 'case 0x\1: return \3;' ;;
  esac
done
