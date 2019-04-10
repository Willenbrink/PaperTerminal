#!/bin/sh
make --directory=EPD;
make;
dune build ./term.exe;
sudo killall term.exe;
sudo `pwd`/_build/default/term.exe $@
