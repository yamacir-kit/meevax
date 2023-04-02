#!/bin/sh

root="$(git rev-parse --show-toplevel)"

current_version()
{
  # count="$(git rev-list --count HEAD)"
  # echo "0.3.$((count - 2135))"

  count="$(git rev-list --no-merges --count HEAD)"
  echo "0.4.$((count - 2854))"
}

update_version()
{
  current_version > "$root/VERSION"
}

list_version()
{
  git fetch origin --tags
  git tag --list --sort=version:refname | sed -e 's/^/  /'
  echo "\e[32m* v$(current_version)\e[0m"
}

update_version
