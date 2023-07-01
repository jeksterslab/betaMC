#!/bin/bash

git clone git@github.com:jeksterslab/betaMC.git
rm -rf "$PWD.git"
mv betaMC/.git "$PWD"
rm -rf betaMC
