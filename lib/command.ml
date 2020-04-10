(* Second layer: Send commands to the IT8951 *)

(* TODO we could in theory also encode the parameters of the command
 * to prevent invalid read/writes.
 *)
let int_of_cmd = function
  | `Sys_run -> 0x1
  | `Standby -> 0x2
  | `Sleep -> 0x3
  | `Register_read -> 0x10
  | `Register_write -> 0x11
  (* TODO Strange note in the datasheet regarding:
   * Burst_read_trigger, burst_write, load_image, load_image_area
   * "For these commands, the parameters are unnecessary when bit 0 of I80CPCR is false."
   * How many bytes will be read/written when this is not set? Until the bus is closed?
   *)
  | `Burst_read_trigger -> 0x12
  | `Burst_read_start -> 0x13
  | `Burst_write -> 0x14
  | `Burst_end -> 0x15
  | `Load_image -> 0x20
  | `Load_image_area -> 0x21
  | `Load_image_end -> 0x22
  | `DPY_area -> 0x34 (* TODO DPY? *)
  | `Dev_info -> 0x302
  | `DPY_Buf_Area -> 0x37
  | `VCOM -> 0x39

(* TODO annotate types: 8/16/32 bits *)
(* TODO investigate whether we can transmit more than 2 bytes at once *)

let write_cmd cmd =
  Bus.(
    open_bus ();
    send 0x6000;
    send (int_of_cmd cmd);
    close_bus ()
  )

let write_data (data : int list) =
  Bus.(
    open_bus ();
    send 0x0000;
    List.iter send data;
    close_bus ();
  )

let write_data_array send_f =
  Bus.(
    open_bus ();
    send 0x0000;
    send_f send;
    close_bus ()
  )

let read_data amount =
  Bus.(
    open_bus ();
    send 0x1000;
    (* Dummy value, necessary according to data sheet *)
    recv () |> ignore;
    let retVal =
      List.init amount (fun _ -> recv ())
    in
    close_bus ();
    retVal
  )

(* Only wraps read_data so the caller can use the datum without matching *)
let read_datum () =
  match read_data 1 with
  | x::[] -> x
  | _ -> failwith "Error: read_data 1 returns list with length != 1"

let write_cmd_args cmd args =
  write_cmd cmd;
  write_data args

(* Comments are copied from the sample code. *)
let rec addr_of_reg = function
  | `DISPLAY_BASE -> 0x1000 (* Register RW access for I80 only *)
  | `LUT0EWHR -> addr_of_reg `DISPLAY_BASE + 0x00 (* LUT0 Engine Width Height Reg *)
  | `LUT0XYR -> addr_of_reg `DISPLAY_BASE + 0x40 (* LUT0 XY Reg *)
  | `LUT0BADDR -> addr_of_reg `DISPLAY_BASE + 0x80 (* LUT0 Base Address Reg *)
  | `LUT0MFN -> addr_of_reg `DISPLAY_BASE + 0xC0 (* LUT0 Mode and Frame number Reg *)
  | `LUT01AF -> addr_of_reg `DISPLAY_BASE + 0x114 (* LUT0 and LUT1 Active Flag Reg *)
  | `UP0SR -> addr_of_reg `DISPLAY_BASE + 0x134 (* Update Parameter0 Setting Reg *)
  | `UP1SR -> addr_of_reg `DISPLAY_BASE + 0x138 (* Update Parameter1 Setting Reg *)
  | `UP1SR2 -> addr_of_reg `UP1SR + 0x2 (* Update Parameter1 Setting Reg *)
  | `LUT0ABFRV -> addr_of_reg `DISPLAY_BASE + 0x13C (* LUT0 Alpha blend and Fill rectangle Value *)
  | `UPBBADDR -> addr_of_reg `DISPLAY_BASE + 0x17C (* Update Buffer Base Address *)
  | `LUT0IMXY -> addr_of_reg `DISPLAY_BASE + 0x180 (* LUT0 Image buffer X/Y offset Reg *)
  | `LUTAFSR -> addr_of_reg `DISPLAY_BASE + 0x224 (* LUT Status Reg (status of All LUT Engines) *)
  | `BGVR -> addr_of_reg `DISPLAY_BASE + 0x250 (* Bitmap (1bpp) image color table *)
  | `SYS_BASE -> 0x0000
  (* Address of System Registers *)
  | `I80CPCR -> addr_of_reg `SYS_BASE + 0x04
  (* Memory Converter Registers *)
  | `MCSR -> 0x0200
  | `LISAR -> addr_of_reg `MCSR + 0x08
  | `LISAR2 -> addr_of_reg `LISAR + 0x02 (* Added here because we do not want addition to work on registers. *)

(* Could in theory write/read more than 16 bit according to the datasheet.
 * We don't use this functionality and instead write to multiple registers (LISAR/LISAR2)
 *)
let write_reg reg value =
  write_cmd_args `Register_write [addr_of_reg reg; value]

let read_reg reg =
  write_cmd_args `Register_read [addr_of_reg reg];
  read_datum ()

let get_vcom () =
  write_cmd_args `VCOM [0];
  read_datum ()

let set_vcom vcom =
  write_cmd_args `VCOM [1; vcom]
