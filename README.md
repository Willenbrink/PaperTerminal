# PaperTerminal
## Summary
This project provides a library to interact with the Waveshare E-Ink Displays written in OCaml. It is intended to run on a Raspberry Pi and interact with the IT8951 HAT via SPI.
A future goal for this project is to provide executables which explore the usage of this library, some of which could be: a terminal, a VNC client, a kernel module to implement a graphics driver or a custom/adapted GUI library.

It is quite similar to PaperTTY(https://github.com/joukos/PaperTTY) which is implemented in Python and provides both a terminal and VNC client. As this is a personal hobby PaperTTY is superior in almost all aspects.

## Structure
The source code is split into three directories:
* lib: contains the library to interact with the display, more details below
* bin: contains the executables which use the library, currently only contains the terminal emulator
* tests: contains all the tests

The library is split into four layers, each of which is concerned with a specific abstraction:
* Bus and bus.c/h: Implements the SPI interface with the IT8951
* Command: Implements the commands defined by the IT8951 and memory/register access
* Controller and State: Provides an interface to interact with the screen on a simplified level: It allows transmitting the buffer to the IT8951 and displaying it onto the E-Ink Display.
* EPD and Bresenham: Provides facilities to quickly test the library, i.e. plotting dots and lines. This layer will perhaps be moved into the bin directory.

## Dependencies
* [bcm2835 library](http://www.airspayce.com/mikem/bcm2835/)
* OCaml compiler
* dune, ctypes, ctypes-foreign (see `dune external-lib-deps $build_target`)
* utop for interactive usage

## Building
Run either of the following commands. Root privileges are necessary to access the pins.
* `dune exec bin/term.exe` for the terminal emulator
* `dune utop lib` for interactive utop REPL with the library
* `dune test` to run the tests specified in tests/dune

## Additional Notes
My goal is to run this project on the Raspberry Pi 3A+. My distro of choice is Void Linux. Unfortunately Void does not have good support of the newer Pis and getting it to run involved some work. I chose to run U-Boot first to enable some flexibility in the future (Network boot, USB boot, independent layer below Linux which always works). This is in theory quite simple:
* Flash the Linux image
* Place the U-Boot image (see below) alongside the kernel in the /boot partition/directory
* Append to config.txt:\
kernel=u-boot.img\
(the following are perhaps unnecessary)\
arm_64bit=1\
device_tree_address=0x100\
device_tree_end=0x8000\
* Freeze the kernel and firmware as updates to them break the system via `xbps-pkgdb -m hold rpi-firmware rpi-kernel`

Be careful to gracefully shut down the Pi at all times as power loss corrupts the SD card (as indicated by the fsck after reboot)

## Creating the U-Boot image
The mainline U-Boot repository has support for most Raspberry Pis and their features.
The two main configuration options relevant here are:\
`CONFIG_BOOTARGS="root=/dev/mmcblk0p2 rw rootwait console=ttyAMA0,115200 kgdboc=ttyAMA0,115200 console=tty1 smsc95xx.turbo_mode=N dwc_otg.lpm_enable=0 loglevel=4 elevator=noop"`\
and\
`CONFIG_BOOTCOMMAND="setenv fdtfile bcm2710-rpi-3-b-plus.dtb; mmc dev 0; fatload mmc 0:1 ${kernel_addr_r} kernel.img; fatload mmc 0:1 ${fdt_addr_r} ${fdtfile}; bootefi ${kernel_addr_r} ${fdt_addr_r}"`\
