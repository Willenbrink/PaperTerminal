(* Second layer: Send commands to the IT8951 *)

(* Comments are copied from the sample code. *)
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
  | SYS_BASE
  (* Address of System Registers *)
  | I80CPCR
  (* Memory Converter Registers *)
  | MCSR
  | LISAR
  | LISAR2 (* Added here because we do not want addition to work on registers. *)

(* Comments are copied from the sample code. *)
let rec addr_of_reg = function
  | DISPLAY_BASE -> 0x1000 (* Register RW access for I80 only *)
  | LUT0EWHR -> addr_of_reg DISPLAY_BASE + 0x00 (* LUT0 Engine Width Height Reg *)
  | LUT0XYR -> addr_of_reg DISPLAY_BASE + 0x40 (* LUT0 XY Reg *)
  | LUT0BADDR -> addr_of_reg DISPLAY_BASE + 0x80 (* LUT0 Base Address Reg *)
  | LUT0MFN -> addr_of_reg DISPLAY_BASE + 0xC0 (* LUT0 Mode and Frame number Reg *)
  | LUT01AF -> addr_of_reg DISPLAY_BASE + 0x114 (* LUT0 and LUT1 Active Flag Reg *)
  | UP0SR -> addr_of_reg DISPLAY_BASE + 0x134 (* Update Parameter0 Setting Reg *)
  | UP1SR -> addr_of_reg DISPLAY_BASE + 0x138 (* Update Parameter1 Setting Reg *)
  | UP1SR2 -> addr_of_reg UP1SR + 0x2 (* Update Parameter1 Setting Reg *)
  | LUT0ABFRV -> addr_of_reg DISPLAY_BASE + 0x13C (* LUT0 Alpha blend and Fill rectangle Value *)
  | UPBBADDR -> addr_of_reg DISPLAY_BASE + 0x17C (* Update Buffer Base Address *)
  | LUT0IMXY -> addr_of_reg DISPLAY_BASE + 0x180 (* LUT0 Image buffer X/Y offset Reg *)
  | LUTAFSR -> addr_of_reg DISPLAY_BASE + 0x224 (* LUT Status Reg (status of All LUT Engines) *)
  | BGVR -> addr_of_reg DISPLAY_BASE + 0x250 (* Bitmap (1bpp) image color table *)
  | SYS_BASE -> 0x0000
  (* Address of System Registers *)
  | I80CPCR -> addr_of_reg SYS_BASE + 0x04
  (* Memory Converter Registers *)
  | MCSR -> 0x0200
  | LISAR -> addr_of_reg MCSR + 0x08
  | LISAR2 -> addr_of_reg LISAR + 0x02 (* Added here because we do not want addition to work on registers. *)

type _ command =
  | Wakeup : unit command
  | Standby : unit command
  | Sleep : unit command
  | Reg_read : register -> int command
  | Reg_write : register * int -> unit command
  | Burst_read : int * int -> int list command
  | Burst_write : int * int list -> unit command
  | Load_image : int * (int * int * int * int) option * ((int -> unit) -> unit) -> unit command
  (* TODO sample code states: Currently only one buffer. *)
  | Display_area : int * (int * int * int * int) option * int option -> unit command
  | VCOM_read : int command
  | VCOM_write : int -> unit command
  | Query_info : unit command

type command_internal =
  | Wakeup'
  | Standby'
  | Sleep'
  | Reg_read'
  | Reg_write'
  | Burst_read_trigger' (* TODO can be hidden as it only occurs directly before Burst_read_start*)
  | Burst_read_start'
  | Burst_write'
  | Burst_end'
  | Load_image'
  | Load_image_area'
  | Load_image_end'
  | Display_area'
  | Display_area_buffer'
  | VCOM'
  | Query_info'

let int_of_cmd = function
  | Wakeup' -> 0x1
  | Standby' -> 0x2
  | Sleep' -> 0x3
  | Reg_read' -> 0x10
  | Reg_write' -> 0x11
  (* TODO Strange note in the datasheet regarding:
   * Burst_read_trigger, burst_write, load_image, load_image_area
   * "For these commands, the parameters are unnecessary when bit 0 of I80CPCR is false."
   * How many bytes will be read/written when this is not set? Until the bus is closed?
   *)
  | Burst_read_trigger' -> 0x12
  | Burst_read_start' -> 0x13
  | Burst_write' -> 0x14
  | Burst_end' -> 0x15
  | Load_image' -> 0x20
  | Load_image_area' -> 0x21
  | Load_image_end' -> 0x22
  | Display_area' -> 0x34
  | Display_area_buffer' -> 0x37
  | VCOM' -> 0x39
  | Query_info' -> 0x302

(* TODO annotate types: 8/16/32 bits *)

(* TODO rename *)
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

let write_cmd_args cmd args =
  write_cmd cmd;
  write_data args

let write_data_array array_iter =
  Bus.(
    open_bus ();
    send 0x0000;
    array_iter send;
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



let split_32 value =
  assert (value land 0x7FFFFFFF00000000 == 0);
  (value land 0xFFFF, (value lsr 16) land 0xFFFF)

let write_cmd : type a. a command -> a = function
  | Query_info -> write_cmd Query_info'
  | Wakeup | Standby | Sleep as cmd ->
    ignore cmd;
    failwith "Undefined"
  | Reg_read reg ->
    write_cmd_args Reg_read' [addr_of_reg reg];
    read_datum ()
  | Reg_write (reg, value) ->
    write_cmd_args Reg_write' [addr_of_reg reg; value]
  | Burst_read (address, amount) ->
    let (addrH,addrL) = split_32 address in
    let (amountH,amountL) = split_32 amount in
    write_cmd_args Burst_read_trigger' [addrH;addrL;amountH;amountL];
    write_cmd Burst_read_start';
    let ret : int list = read_data amount in
    write_cmd Burst_end';
    ret
  | Burst_write (address, data) ->
    let (addrH,addrL) = split_32 address in
    let (amountH,amountL) = split_32 (List.length data) in
    write_cmd_args Burst_write' ([addrH;addrL;amountH;amountL] @ data)
  | Load_image (info,area,array_iter) ->
    begin
      match area with
      | None ->
        write_cmd_args Load_image' [info]
      | Some (x,y,w,h) ->
        write_cmd_args Load_image_area' [info;x;y;w;h]
    end;
    write_data_array array_iter;
    write_cmd Load_image_end'
  | Display_area (mode,area,address) ->
    let x,y,w,h = match area with
      | Some area -> area
      | None -> (0,0,800,600) (* TODO hardcoded! *)
    in
    begin
      match address with
      | None ->
        write_cmd_args Display_area' [x;y;w;h;mode]
      | Some address ->
        let (addrH,addrL) = split_32 address in
        write_cmd_args Display_area_buffer' [x;y;w;h;mode;addrH;addrL]
    end
  | VCOM_write value ->
    write_cmd_args VCOM' [1;value]
  | VCOM_read ->
    write_cmd_args VCOM' [0];
    read_datum ()

(* TODO move: Could in theory write/read more than 16 bit according to the datasheet.
 * We don't use this functionality and instead write to multiple registers (LISAR/LISAR2)
 *)
