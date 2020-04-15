(** First layer: Interact with the bus and transfer bytes.

    This module implements basic SPI functionality. This is used by the layers above to interact with the IT8951.
    A basic example would be:
    {[init (); open_bus (); send 0x00; wait_for_bus (); recv () |> print_int; close_bus (); free ()]}
    It is written in an imperative style and will raise exceptions to indicate problems with the SPI bus or invalid instruction order.
    For more information regarding the SPI bus or the BCM2835 refer to the {{: http://www.airspayce.com/mikem/bcm2835/} BCM2835 library} on which this module builds.
*)

val init : unit -> bool
(** [init ()] initialises the BCM2835 and sets the mode to SPI. Returns [true] on success. Only call this function once. *)

val free : unit -> unit
(** [free ()] frees the resources allocated by BCM2835 and enables another program to access it. Only call this function after a successfull [init ()]. *)

val open_bus : unit -> unit
(** TODO verify: [open_bus ()] waits until the bus is available and signals the IT8951 to receive the following data. *)

val close_bus : unit -> unit
(** TODO [close_bus ()] should be called after sending data. *)

val wait_for_bus : unit -> unit
(** [wait_for_bus ()] blocks until the IT8951 is ready to receive/send data. *)

val send : int -> unit
(** [send value] sends the rightmost 16-bit of [value]. The remaining bits of [value] are silently ignored even when not 0. It throws an exception TODO when the IT8951 simultaneously sends data (unequal 0). This ensures that an erroneous command is detected early. *)

val recv : unit -> int
(** [recv ()] returns a 16-bit value. *)
