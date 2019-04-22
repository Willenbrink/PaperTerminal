# PaperTerminal
This project enables you to use the IT8951 HAT to display a terminal on an E-Paper Display (EPD).
In this respect it's quite similar to PaperTTY(https://github.com/joukos/PaperTTY) which is implemented in Python.

Improvements made by this project:
* Support for the IT8951
* Performance as its written in a compiled language
* Better drawing algorithm than a simple bounding box (draw all connected dirty areas at once but draw unconnected areas separately)
* Done by myself as a hobby. Thats the primary reason ;-)

Lacking features:
* Fonts are not supported
* UTF-8 is not supported
* Resizing the terminal is currently not possible

# Make
Before compiling the project, add a softlink to a folder in the IT8951 project named either eink-display or emulator depending on what functionality you want.
"ln -s ~/Path/To/IT8951/EPD ./EPD" should do the job.
This structure is not set in stone and will likely change.

Execute run.sh. It should automatically compile both the C and OCaml source and execute the program.
Due to the fact that the display can only be accessed by one program at once it calls "killall term.exe".

# Dependencies
* OCaml (>= 4.02)
* GCC
* ctypes and ctypes.foreign (see dune in project root)
* EPD, provided by my fork of the IT8951 repository
