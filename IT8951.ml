open Bcm2835

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
  | VCOM

let init () = init ()
let free () = free ()
let write preamble data =
  open_bus ();
  transfer(preamble);
  wait_for_bus ();
  List.iter transfer data;
  close_bus ()

let write_data data = write 0x0000 data
let read_data amount =
  open_bus ();
  transfer preamble;
  wait_for_bus ();
  transfer 0x00; (* Dummy value, is this necessary? *)
  wait_for_bus ();
  let retVal =
    List.init amount (fun _ -> 0)
    |> List.map transfer
  in
  close_bus ();
  retVal

let write_cmd (cmd : command) = write 0x6000 [cmd]
let write_cmd_args cmd args =
  write_cmd cmd;
  List.iter write_data args
