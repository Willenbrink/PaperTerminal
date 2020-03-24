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

# Additional Notes
My goal is to run this project on the Raspberry Pi 3A+. Additionally my preferred distro is Voidlinux. Because Void does not have as good support of the Pis as I wished getting it to run involved some work. I chose to run U-Boot first to enable some flexibility in the future (Network boot, USB boot, independent layer below Linux which always works). This is in theory quite simple:
* Flash the Linux image
* Place the U-Boot image (see below) alongside the kernel in the /boot partition/directory
* Modify config.txt: "kernel=u-boot.img" and "arm_64bit=1\ndevice_tree_address=0x100\ndevice_tree_end=0x8000" (perhaps these are unnecessary as this should already be covered by the configuration below)
* Freeze the kernel and firmware as updates to them break the system via `xbps-pkgdb -m hold rpi-firmware rpi-kernel`
* Update and ignore the errors fsck reports for the SD card (where do they even come from? f3 states that the card is fine...)

## Creating the U-Boot image
The mainline U-Boot repository has support for most Raspberry Pis and their features.
The two main configuration options relevant here are:\
`CONFIG_BOOTARGS="root=/dev/mmcblk0p2 rw rootwait console=ttyAMA0,115200 kgdboc=ttyAMA0,115200 console=tty1 smsc95xx.turbo_mode=N dwc_otg.lpm_enable=0 loglevel=4 elevator=noop"`\
and\
`CONFIG_BOOTCOMMAND="setenv fdtfile bcm2710-rpi-3-b-plus.dtb; mmc dev 0; fatload mmc 0:1 ${kernel_addr_r} kernel.img; fatload mmc 0:1 ${fdt_addr_r} ${fdtfile}; bootefi ${kernel_addr_r} ${fdt_addr_r}"`\
