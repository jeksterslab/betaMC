#!/bin/bash

# the SSH host: github-<owner> when ~/.ssh/config defines that alias (for
# example on Roar), else github.com
host="github.com"
if grep -Eqs "^Host[[:space:]]+(.*[[:space:]])?github-jeksterslab([[:space:]]|$)" ~/.ssh/config; then
  host="github-jeksterslab"
fi
git clone "git@${host}:jeksterslab/betaMC.git"
rm -rf "$PWD/.git"
mv "betaMC/.git" "$PWD"
rm -rf "betaMC"
