#  Copyright 2018-2025 Tatsuya Yamasaki.
#
#  Licensed under the Apache License, Version 2.0 (the "License");
#  you may not use this file except in compliance with the License.
#  You may obtain a copy of the License at
#
#      http://www.apache.org/licenses/LICENSE-2.0
#
#  Unless required by applicable law or agreed to in writing, software
#  distributed under the License is distributed on an "AS IS" BASIS,
#  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
#  See the License for the specific language governing permissions and
#  limitations under the License.

BEGIN {
  RS = "###"
}

{
  s = $0

  n = length(s)

  i = 0

  while (i < n)
  {
    c = substr(s, ++i, 1)

    if (c == "\"")
    {
      printf "\""

      do
      {
        c = substr(s, ++i, 1)

        if (c == "\\")
        {
          printf "\\%c", substr(s, ++i, 1) # Print escape sequence
        }
        else
        {
          printf "%c", c
        }
      }
      while (c != "\"" && i < n)
    }
    else if (c == "#")
    {
      c = substr(s, ++i, 1)

      if (c == "|")
      {
        while (substr(s, ++i, 2) != "|#" && i < n) {} # Discard block comment

        ++i # Discard last sharp

        while (substr(s, i + 1, 1) ~ /[ \n]/ && ++i < n) {} # Discard intraline whitespace
      }
      else if (c == "\\")
      {
        printf "#\\%c", substr(s, ++i, 1) # Print character literal
      }
      else
      {
        printf "#%c", c # Print #t, #f
      }
    }
    else if (c == ";")
    {
      while (substr(s, i + 1, 1) != "\n" && ++i < n) {} # Discard line comment

      while (substr(s, i + 1, 1) ~ /[ \n]/ && ++i < n) {} # Discard intraline whitespace
    }
    else if (c == "\n")
    {
      if (i < n)
      {
        printf " "
      }

      while (substr(s, i + 1, 1) ~ /[ \n]/ && ++i < n) {} # Discard indent
    }
    else
    {
      printf "%c", c
    }
  }
}
