#!/bin/sh -eu

working_directory=$(pwd -P)
repository="$(git rev-parse --show-toplevel)"

compile='g++-7'
execute=0
valgrind=''
process=1
purpose='Debug'
clean_build=0

echo "
; ==== Overview ================================================================
;
; Drectory
;   repository      = $repository
;   current-working = $working_directory
;
; Configuration"

for each in "$@"
do
  case $each in
    -a | --all )
      all=1
      printf ';   all\t\t= %s\n' "$all"
      shift
      ;;

    -b | --build )
      clean_build=1
      printf ';   clean-build\t= %s\n' "$clean_build"
      shift
      ;;

    -c | --clang )
      compile='clang++-6.0'
      printf ';   compile\t= %s\n' "$compile"
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

    --file=* )
      file="${each#*=}"
      printf ';   file\t= %s\n' "$file"
      shift
      ;;

    -g | --gcc )
      compile="g++-7"
      printf ';   compile\t= %s\n' "$compile"
      shift
      ;;

    -v | --valgrind )
      valgrind="valgrind -v --leak-check=full --show-leak-kinds=all --log-file=$repository/build/valgrind.cpp"
      printf ';   valgrind\t= %s\n' "$valgrind"
      shift
      ;;

    -j* )
      process="${each#*j}"
      printf ';   process\t= %s' "$process"

      if test "$process" = "0"
      then
        process="$(nproc --all)"
        printf ' => %s' "$process"
      fi

      echo
      shift
      ;;

    --process=* )
      process="${each#*=}"
      printf ';   process\t= %s' "$process"

      if test "$process" = "auto"
      then
        process="$(nproc --all)"
        printf ' => %s' "$process"
      fi

      echo
      shift
      ;;

    -r | --release )
      purpose='Release'
      printf ';   purpose\t= %s\n' "$purpose"
      shift
      ;;

    * )
      shift
      ;;
  esac
done

echo ";
; ==============================================================================
"

clean()
{
  echo "
; ==== Clean ===================================================================
;
; Command"

  if test -n "$(ls "$repository/build")"
  then
    echo ";   rm -rf    $repository/build"
              rm -rf   "$repository/build"
  fi

  echo ";   mkdir -p  $repository/build"
            mkdir -p "$repository/build"

  echo ";   cd        $repository/build"
            cd       "$repository/build"

  echo ";
; ==============================================================================
"
}

build()
{
  echo "
; ==== CMake ===================================================================
;
; Command
;   cmake .. -DCMAKE_BUILD_TYPE=$purpose -DCMAKE_CXX_COMPILER=$compile
;
; ==============================================================================
"

  cmake .. -DCMAKE_BUILD_TYPE="$purpose" -DCMAKE_CXX_COMPILER="$compile"

  echo "
; ==== Make ====================================================================
;
; Command
;   make -j$process
;
; ==============================================================================
"

  make -j"$process"
}

if test "$clean_build" -ne 0
then
  clean
  build
fi

count()
{
  n="$1"

  while test "$n" -ge 0
  do
    printf "\r\e[K;  It starts after %d seconds" "$n"
    n=$(( n - 1 ))
    sleep 1
  done

  printf "\n"
}

if test "$execute" -ne 0
then
  command="$valgrind $repository/build/bin/ice --verbose --debug --interactive"

  echo "
; ==== Test ====================================================================
;
; command = $command < $repository/test.obsoleted/test.scm
;"

  count 5

  echo ";
; ==============================================================================
"
  $command < "$repository/test.obsoleted/test.scm"
fi

