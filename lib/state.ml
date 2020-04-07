let dev_info : Cmd.device_info option ref = ref None

let set_dev_info value = dev_info := Some value
let get_dev_info () = match !dev_info with
  | Some x -> x
  | None -> failwith "No device_info set"

let buffer : int array array option ref = ref None

let set_buffer value = buffer := Some value
let get_buffer () = match !buffer with
  | Some x -> x
  | None -> failwith "No buffer set"
