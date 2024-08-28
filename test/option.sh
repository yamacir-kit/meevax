#!/bin/sh

meevax=$1

$meevax -e '(import (scheme base))' -e '(+ 1 2 3)'
$meevax -h
$meevax -v
$meevax -w '(+ 1 2 3)'
