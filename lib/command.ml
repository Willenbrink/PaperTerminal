(* Second layer: Send commands to the IT8951 *)

let int_of_cmd = function
  | `Sys_run -> 0x1
  | `Standby -> 0x2
  | `Sleep -> 0x3
  | `Register_read -> 0x10
  | `Register_write -> 0x11
  | `Burst_read_t -> 0x12 (* TODO t? *)
  | `Burst_read_s -> 0x13
  | `Burst_write -> 0x14
  | `Burst_end -> 0x15
  | `Load_image -> 0x20
  | `Load_image_area -> 0x21
  | `Load_image_end -> 0x22
  | `DPY_area -> 0x34 (* TODO DPY? *)
  | `Dev_info -> 0x302
  | `DPY_Buf_Area -> 0x37
  | `VCOM -> 0x39
  | _ -> failwith "Undefined command, check the spelling?"

(* TODO annotate types: 8/16/32 bits *)
(* TODO investigate whether we can transmit more than 2 bytes at once *)
let write (preamble : int) (data : int) =
  Bus.(
    open_bus ();
    send preamble;
    wait_for_bus ();
    send data;
    close_bus ()
  )

let write_data (data : int list) =
  List.iter (write 0x0000) data

let read_data amount =
  Bus.(
    open_bus ();
    send 0x1000;
    wait_for_bus ();
    send 0x00; (* TODO Dummy value, is this necessary? *)
    wait_for_bus ();
    let retVal =
      List.init amount (fun _ -> transfer 0)
    in
    close_bus ();
    retVal
  )

let read_datum () =
  match read_data 1 with
  | x::[] -> x
  | _ -> failwith "Invalid read"

let write_cmd cmd =
  write 0x6000 (int_of_cmd cmd)

let write_cmd_args cmd (args : int list) =
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

let read_reg reg =
  write_cmd_args `Register_read [addr_of_reg reg];
  read_datum ()

let write_reg reg value =
  write_cmd_args `Register_write [addr_of_reg reg; value]
