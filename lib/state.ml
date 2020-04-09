type device_info = { width : int; height : int; address : int; fwversion : string; lutversion : string }

let dev_info : device_info option ref = ref None

let set_dev_info value = dev_info := Some value
let get_dev_info () = match !dev_info with
  | Some x -> x
  | None -> failwith "No device_info set"

(* TODO this can likely be improved with functors
 * Have a module containing the device info or width/height
 * Implement a functor taking that module and creating an array accordingly
 * This would do away with that "option ref"
 *)

type array = (int, Bigarray.int8_unsigned_elt, Bigarray.c_layout) Bigarray.Array2.t

let buffer : array option ref = ref None

let set_buffer value = buffer := Some value
let get_buffer () = match !buffer with
  | Some x -> x
  | None -> failwith "No buffer set"
