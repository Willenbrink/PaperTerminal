(** Third layer: Control the IT8951 by sending commands *)

type mode =
  | White
  | Fast1BPP
  | Slow
  | Medium
  | Fast

(** TODO *)

val init : unit -> unit
(** [init ()] Initialises the library and raises an exception when it fails. It queries the IT8951 for screen dimensions and then allocates a buffer of the correct size. The initialised values are saved in the State module TODO until a better solution is implemented. *)

val free : unit -> unit
(** [free ()] Frees the allocated resources.*)

val transmit : (int * int * int * int) -> unit
(** TODO use Area module [transmit (x,y,w,h)] transmits an area of the buffer to the IT8951. This area is not displayed immediately. [x] and [y] refer to the upper left corner of the displayed area, [w] and [h] to the width and height respectively. To display the area use the display function. *)

val display : (int * int * int * int) -> mode -> unit
(** [display (x,y,w,h) mode] displays the area specified by (x,y,w,h) and passes the [mode] along to determine the display speed/clarity. *)
