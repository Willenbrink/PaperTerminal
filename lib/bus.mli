(** First layer: Interact with the bus and transfer bytes.

    This module implements basic SPI functionality. This is used by the layers above to interact with the IT8951. It is written in an imperative style and will raise exceptions to indicate problems with the SPI bus. There are almost no failsafes implemented so correct usage is advised.
    A basic example that sends 0xFF and receives the response would be:
    {[init ();
      open_bus ();
      send 0xFF;
      recv ()
      |> print_int;
      close_bus ();
      free ()]}
    For more information regarding the SPI bus or the BCM2835 refer to the {{: http://www.airspayce.com/mikem/bcm2835/} BCM2835 library} on which this module builds.
*)

val init : unit -> unit
(** [init ()] initialises the BCM2835 and sets the mode to SPI. Returns () on success and raises a [Failure exception] otherwise. Only call this function once. *)

val free : unit -> unit
(** [free ()] frees the resources allocated by BCM2835 and enables another program to access it. Only call this function after a successfull [init ()]. *)

val open_bus : unit -> unit
(** [open_bus ()] signals the IT8951 to receive the following data. Must be called before calling send/recv. *)

val close_bus : unit -> unit
(** [close_bus ()] signals the end of transmission. Must be called once a significant pause in transmission is assumed. *)

val send : int -> unit
(** [send value] sends the rightmost 16-bit of [value]. If any of the upper bits of [value] are set, a [Failure exception] is raised. It also raises a [Failure exception] when the IT8951 simultaneously sends data as this is the result of an erroneous command.
TODO Will perhaps be reworked to send a multiple of 8 bit or work on 64 bit. *)

val recv : unit -> int
(** [recv ()] returns a 16-bit value. *)
