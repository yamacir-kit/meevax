#!/bin/sh

root="$(git rev-parse --show-toplevel)"

current_version()
{
  count="$(git rev-list --count HEAD)"
  echo "0.3.$((count - 2135))"
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

update_version && list_version
