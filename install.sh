#!/usr/bin/env bash

set -e

HERE=$(cd `dirname $0`; pwd)

cd $HERE
cabal sandbox init
cabal sandbox add-source ../text-zipper
cabal install -j
