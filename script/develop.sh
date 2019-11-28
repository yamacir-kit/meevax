#!/bin/sh -eu

working_directory=$(pwd)
repository="$(git rev-parse --show-toplevel)"

compile='g++-7'
execute=0
memory_check=''
process=1
purpose='Debug'
rebuild=0

echo "
; ==== Meevax Develop Script ==================================================
;
; Informations
;   repository        = $repository
;   working-directory = $working_directory
;
; Configurations"

for each in "$@"
do
  case $each in
    -b | --build )
      rebuild=1
      printf ';   rebuild\t= %s\n' "$rebuild"
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

    -f=* | --file=* )
      file="${each#*=}"
      printf ';   file\t= %s\n' "$file"
      shift
      ;;

    -g | --gcc )
      compile="g++-7"
      printf ';   compile\t= %s\n' "$compile"
      shift
      ;;

    -m | --memory-check )
      memory_check="valgrind --leak-check=full --log-file=$repository/build/memory_check.cpp"
      printf ';   memcheck\t= %s\n' "$memory_check"
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

echo ';
; ============================================================================='

if test "$rebuild" -ne 0
then
  mkdir -vp "$repository/build"
  cd "$repository/build"

  if test -e "$repository/build/Makefile"
  then
    make clean
  fi

  cmake .. \
    -DCMAKE_BUILD_TYPE="$purpose" \
    -DCMAKE_CXX_COMPILER="$compile"

  make -j"$process"
fi

if test "$execute" -ne 0
then
  command="$memory_check $repository/build/bin/meevax --verbose"

  echo "
; ==== Execution ==============================================================
;
; command = $command < $repository/test/test.scm
;
; =============================================================================
"
  $command < "$repository/test/test.scm"
fi

