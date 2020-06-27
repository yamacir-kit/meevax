#!/bin/sh -eu

working_directory=$(pwd -P)
repository="$(git rev-parse --show-toplevel)"

clean_build=0
compile='g++-7'
job=1
purpose='Debug'
autotest=0

valgrind=''
valgrind_options='--verbose --leak-check=full --show-leak-kinds=all --error-exitcode=1'

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

    -j* )
      job="${each#*j}"
      printf ';   job\t= %s' "$job"

      if test "$job" = "0"
      then
        job="$(nproc --all)"
        printf ' => %s' "$job"
      fi

      echo
      shift
      ;;

    --job=* )
      job="${each#*=}"
      printf ';   job\t= %s' "$job"

      if test "$job" = "auto" -o "$job" = "0"
      then
        job="$(nproc --all)"
        printf ' => %s' "$job"
      fi

      echo
      shift
      ;;

    -t | --test )
      autotest=1
      printf ';   auto-test\t= %s\n' "$autotest"
      shift
      ;;

    -v | --valgrind )
      valgrind="valgrind $valgrind_options --log-file=$repository/build/full-test.leak-check.cpp"
      printf ';   valgrind\t= %s\n' "$valgrind"
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
;   make -j$job
;
; ==============================================================================
"

  make -j"$job"
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

if test "$autotest" -ne 0
then
  unit_test="valgrind $valgrind_options --log-file=$repository/build/exaple.leak-check.cpp $repository/build/bin/example"

  full_test=" \
    $valgrind $repository/build/bin/ice --verbose --debug \
    $repository/experimental/srfi-78.ss \
    $repository/test/r4rs.ss \
    "

  chibi_test=" \
    $valgrind $repository/build/bin/ice \
    $repository/experimental/srfi-78.ss \
    $repository/test/chibi-basic.ss \
    "

  echo "
; ==== Test ====================================================================
;
; Unit Test
;   $unit_test
;
; Full Test
;   $full_test
;"

  count 5

  echo ";
; ==============================================================================
"
  $unit_test
  $full_test
  # $chibi_test
fi
