(** Second layer: Send commands to the IT8951 **)

type register =
  | DISPLAY_BASE (* Register RW access for I80 only *)
  | LUT0EWHR (* LUT0 Engine Width Height Reg *)
  | LUT0XYR (* LUT0 XY Reg *)
  | LUT0BADDR (* LUT0 Base Address Reg *)
  | LUT0MFN (* LUT0 Mode and Frame number Reg *)
  | LUT01AF (* LUT0 and LUT1 Active Flag Reg *)
  | UP0SR (* Update Parameter0 Setting Reg *)
  | UP1SR (* Update Parameter1 Setting Reg *)
  | UP1SR2 (* Update Parameter1 Setting Reg *)
  | LUT0ABFRV (* LUT0 Alpha blend and Fill rectangle Value *)
  | UPBBADDR (* Update Buffer Base Address *)
  | LUT0IMXY (* LUT0 Image buffer X/Y offset Reg *)
  | LUTAFSR (* LUT Status Reg (status of All LUT Engines) *)
  | BGVR (* Bitmap (1bpp) image color table *)
  | SYS_BASE (* Address of System Registers *)
  | I80CPCR
  | MCSR (* Memory Converter Registers *)
  | LISAR
  | LISAR2 (* Added here because we do not want addition to work on registers. *)

type command =
  | Wakeup
  | Standby
  | Sleep
  | Reg_read
  | Reg_write
  | Burst_read_trigger (* TODO can be hidden as it only occurs directly before Burst_read_start*)
  | Burst_read_start
  | Burst_write
  | Burst_end
  | Load_image
  | Load_image_area
  | Load_image_end
  | Display_area
  | Query_info
  | Display_area_buffer (* TODO sample code states: Currently only one buffer. *)
  | VCOM

val write_cmd : command -> unit
(** [write_cmd cmd] sends a command [cmd] to the IT8951. *)

val write_data : int list -> unit
(** [write_data data] sends all elements of [data], starting at the first element, to the IT8951. *)

val write_data_array : ((int -> unit) -> unit) -> unit
(** TODO [write_data_array send_fun] passes the Bus.send function to send_fun. send_fun should then use Bus.send to send all the data contained within the array. Will be redesigned. *)

val read_data : int -> int list
(** [read_data amount] returns a list of values of length [amount] it read from the bus. *)

val read_datum : unit -> int
(** [read_datum ()] returns a single read value. It only wraps read_data so the caller can use the datum without matching. *)

val write_cmd_args : command -> int list -> unit
(** TODO write a command together with argument list. Will be reworked. *)

val write_reg : register -> int -> unit
(** [write_reg reg val] writes the 16 rightmost bits of [val] to the register [reg]. *)

val read_reg : register -> int
(** [read_reg reg] returns the 16 rightmost bits of the register [reg]. *)

val get_vcom : unit -> int
(** TODO VCOM? [get_vcom ()] returns the current voltage applied to the VCOM. *)

val set_vcom : int -> unit
(** [set_vcom val] sets the VCOM voltage to [val]. *)
