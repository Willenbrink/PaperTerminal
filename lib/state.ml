let big_endian = ref false
(* Necessary because Polymorphic Variants don't have a determined type
 * but ref only works with known types.
 * TODO could perhaps be replaced with simple variants
 *)
let bpp : [ `Bpp1 | `Bpp2 | `Bpp3 | `Bpp4 | `Bpp8 ] ref = ref `Bpp1
(* TODO Left/Right not working currently. Buffer must be reallocated to adjust width/height. *)
let rotation : [ `Down | `Left | `Right | `Up ] ref = ref `Down

let int_of_bool = function true -> 1 | false -> 0

let int_of_bpp = function
  (* TODO packing *)
  | `Bpp2 -> 0 (* Packing: 1100_1100 *)
  | `Bpp3 -> 1 (* Packing: 1110_1110 *)
  | `Bpp4 -> 2 (* Packing: 1111_1111 *)
  | `Bpp8 -> 3 (* Packing: 1111_1111 *)
  (* IT8951 does not support 1bpp transfer but we can display the image in 1bpp
   * Therefore we transfer in "8bpp" and then tell the IT8951 to interpret it as 1bpp
   *)
  | `Bpp1 -> 3

let int_of_rot = function
  (* TODO Does the rotation in memory impact performance or anything? *)
  | `Down -> 0
  | `Right -> 1
  | `Up -> 2
  | `Left -> 3

let get_image_info () =
  ((int_of_bool !big_endian) lsl 8)
  lor ((int_of_bpp !bpp) lsl 4)
  lor (int_of_rot !rotation)

type device_info = { width : int; height : int; address : int; fwversion : string; lutversion : string }

let dev_info : device_info option ref = ref None

let set_dev_info value = dev_info := Some value
let get_dev_info () = match !dev_info with
  | Some {width; height; address; fwversion; lutversion} ->
    (*
    let width,height =
      match !rotation with
      | `Down | `Up -> width,height
      | `Left | `Right -> height,width
    in
       *)
    {width; height; address; fwversion; lutversion}

  | None -> failwith "No device_info set"


(* TODO this can likely be improved with functors
 * Have a module containing the device info or width/height
 * Implement a functor taking that module and creating an array accordingly
 * This would do away with that "option ref"
 *)

type array = (int, Bigarray.int8_unsigned_elt, Bigarray.c_layout) Bigarray.Array2.t

let create_buffer x y =
  Bigarray.Array2.create
    Bigarray.int8_unsigned
    Bigarray.C_layout x y

let buffer : array option ref = ref None

let set_buffer value = buffer := Some value
let get_buffer () = match !buffer with
  | Some x -> x
  | None -> failwith "No buffer set"
