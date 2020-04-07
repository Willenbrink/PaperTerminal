(* Second layer of abstraction
   Depends on Bcm2835 *)

type command =
  | Sys_run
  | Standby
  | Sleep
  | Register_read
  | Register_write
  | Burst_read_t (*TODO t?*)
  | Burst_read_s
  | Burst_write
  | Burst_end
  | Load_image
  | Load_image_area
  | Load_image_end
  | DPY_area (* TODO DPY?*)
  | Dev_info
  | DPY_Buf_Area
  | VCOM

let int_of_cmd = function
  | Sys_run -> 0x1
  | Standby -> 0x2
  | Sleep -> 0x3
  | Register_read -> 0x10
  | Register_write -> 0x11
  | Burst_read_t (*TODO t?*) -> 0x12
  | Burst_read_s -> 0x13
  | Burst_write -> 0x14
  | Burst_end -> 0x15
  | Load_image -> 0x20
  | Load_image_area -> 0x21
  | Load_image_end -> 0x22
  | DPY_area (* TODO DPY?*) -> 0x34
  | Dev_info -> 0x302
  | DPY_Buf_Area -> 0x37
  | VCOM -> 0x39

let init () = Bcm.init ()

let free () = Bcm.free ()

(* TODO annotate types: 8/16/32 bits *)
let write preamble data =
  Bcm.open_bus ();
  Bcm.send preamble;
  Bcm.wait_for_bus ();
  Bcm.send data;
  Bcm.close_bus ()

let write_data data =
  List.iter (write 0x0000) data

let read_data amount =
  Bcm.open_bus ();
  Bcm.send 0x1000;
  Bcm.wait_for_bus ();
  Bcm.send 0x00; (* TODO Dummy value, is this necessary? *)
  Bcm.wait_for_bus ();
  let retVal =
    List.init amount (fun _ -> Bcm.transfer 0)
  in
  Bcm.close_bus ();
  retVal

let read_data_single () =
  match read_data 1 with
  | x::[] -> x
  | _ -> failwith "Invalid read"


let write_cmd (cmd : command) =
  write 0x6000 (int_of_cmd cmd)

let write_cmd_args cmd args =
  write_cmd cmd;
  write_data args
