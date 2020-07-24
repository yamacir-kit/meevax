#!/bin/sh -eu

working_directory=$(pwd -P)

root="$(git rev-parse --show-toplevel)"

autotest=0
clean_build=0
install=0
compile='g++-7'
install=0
job=1
purpose='Debug'
uninstall=0

valgrind=''
valgrind_options='--verbose --leak-check=full --show-leak-kinds=all --error-exitcode=1'

echo "
; ==== Overview ================================================================
;
; Drectory
;   root            = $root
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

    -i | --install )
      install=1
      printf ';   install\t= %s\n' "$install"
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

    -u | --uninstall )
      uninstall=1
      printf ';   uninstall\t= %s\n' "$uninstall"
      shift
      ;;

    -v | --valgrind )
      valgrind="valgrind $valgrind_options --log-file=$root/build/full-test.leak-check.cpp"
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
; Command
;   $root/tools/uninstall.sh
;   rm -rf $root/build
;   mkdir -p $root/build
;   cd $root/build
;
; ==============================================================================
"
  $root/tools/uninstall.sh
  rm -rf $root/build
  mkdir -p $root/build
  cd $root/build
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
  clean && build
fi

if test "$install" -ne 0
then
  echo "
; ==== Install ====================================================================
;
; Command
;   sudo make install
;   sudo ldconfig
;
; ==============================================================================
"
  sudo make install
  sudo ldconfig
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
  $root/examples/use-as-embedded-language/test.sh

  full_test=" \
    $valgrind $root/build/bin/ice \
    $root/standard/experimental/srfi-78.ss \
    $root/test/r4rs.ss \
    "

  chibi_test=" \
    $valgrind $root/build/bin/ice \
    $root/standard/experimental/srfi-78.ss \
    $root/test/chibi-basic.ss \
    "

  echo "
; ==== Test ====================================================================
;
; Full Test
;   $full_test
;"

  count 3

  echo ";
; ==============================================================================
"
  $full_test
  # $chibi_test
fi
