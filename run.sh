#!/bin/sh
make --directory=lib;
dune build bin/term.exe;
pkill term.exe;
`pwd`/_build/default/bin/term.exe $@ &
