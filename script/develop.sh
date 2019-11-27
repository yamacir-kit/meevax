#!/bin/sh -eu

root="$(git rev-parse --show-toplevel)"

cxx='g++-7'
purpose='Debug'

echo '
; ==== Meevax Develop Script ==================================================
;
; Command Line Options'

for each in "$@"
do
  case $each in
    -b | --build )
      rebuild=1
      printf ';   rebuild\t= %s\n' "$rebuild"
      shift
      ;;

    -c | --clang )
      cxx='clang++-6.0'
      printf ';   cxx\t= %s\n' "$cxx"
      shift
      ;;

    -d | --debug )
      purpose='Debug'
      printf ';   purpose\t= %s\n' "$purpose"
      shift
      ;;

    -e | --execute )
      execute=1
      printf ';   execute\t= %s\n' "$execute"
      shift
      ;;

    -g | --gcc )
      cxx="g++-7"
      printf ';   cxx\t= %s\n' "$cxx"
      shift
      ;;

    -j=* )
      process="${each#*=}"
      printf ';   process\t= %s' "$process"

      if test "$process" = "auto"
      then
        process="$(nproc --all)"
        printf ' = %s' "$process"
      fi

      echo

      shift
      ;;

    -f=* | --file=* )
      file="${each#*=}"
      printf ';   file\t= %s\n' "$file"
      shift
      ;;

    -r | --release )
      purpose='Release'
      printf ';   purpose\t= %s\n' "$purpose"
      shift
      ;;

    -v | --valgrind )
      valgrind="--valgrind --leak-check=full --log-file=$root/build/leak-check.cpp"
      printf ';   valgrind\t= %s\n' "$valgrind"
      shift
      ;;

    * )
      shift
      ;;
  esac
done

echo ';
; ============================================================================='

if test "$rebuild" -ne 0
then
  mkdir -vp "$root/build"
  cd "$root/build"

  if test -e "$root/build/Makefile"
  then
    make clean
  fi

  cmake .. \
    -DCMAKE_BUILD_TYPE="$purpose" \
    -DCMAKE_CXX_COMPILER="$cxx"

  make -j"$process"
fi

if test "$execute" -ne 0
then
  "$valgrind" "$root/build/bin/meevax" --verbose < "$root/test/test.scm"
fi

