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

type _ command =
  | Wakeup : unit command
  | Standby : unit command
  | Sleep : unit command
  | Reg_read : register -> int command
  | Reg_write : register * int -> unit command
  (* TODO rework to also accept 32 bit values *)
  | Burst_read : int * int -> int list command
  (** [Burst_read address amount] reads from the from the memory location of the IT8951 specified by [address]. The read data is [amount] long. *)
  | Burst_write : int * int list -> unit command
  (** [Burst_write address data] writes [data] to the memory location of the IT8951 specified by [address]. The first element of [data] is written to the address, the second to the next higer etc. *)
  | Load_image : int * (int * int * int * int) option * ((int -> unit) -> unit) -> unit command
  | Display_area : int * (int * int * int * int) option * int option -> unit command
  (** [Display_area mode area address] displays the image stored in memory on the screen. The waveform (i.e. speed/clarity of update) is chosen via [mode]. The area displayed is [area], if specified, otherwise the whole screen is updated. [address] is used to specify which location of the memory is loaded. TODO currently untested *)
  | VCOM_read : int command
  | VCOM_write : int -> unit command
  | Query_info : unit command

val write_cmd : 'a command -> 'a
(** [write_cmd cmd] sends a command [cmd] to the IT8951. *)

val write_data : int list -> unit
(** [write_data data] sends all elements of [data], starting at the first element, to the IT8951. *)

val write_data_array : ((int -> unit) -> unit) -> unit
(** [write_data_array array_iter] passes the Bus.send function to array_iter. array_iter should then iterate over all the values of the array and use Bus.send to send all the data contained within the array. This function will perhaps be redesigned. TODO *)

val read_data : int -> int list
(** [read_data amount] returns a list of values of length [amount] it read from the bus. *)

val read_datum : unit -> int
(** [read_datum ()] returns a single read value. It only wraps read_data so the caller can use the datum without matching. *)
