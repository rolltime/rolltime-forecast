#
# This script will setup R and all its dependencies.
# Its dependencies are managed by `packrat`, an R
# package that takes snapshots of current R working
# projects. This script calls `packrat` that reads
# the scripts requirements and downloads the right
# package binaries.
#
# Author: Luis Capelo | luiscape@gmail.com
#
library(packrat)
status(); restore()
