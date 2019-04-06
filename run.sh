#!/bin/sh
make --directory=IT8951;
make;
dune build ./term.exe;
sudo killall term.exe;
sudo $(pwd)/_build/default/term.exe $@
