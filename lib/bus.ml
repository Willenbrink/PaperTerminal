open Ctypes
open Foreign

(* First layer: Interact with the bus and transfer bytes *)

(* See the dune file in ./lib.
   TODO Currently hardcoded as cwd is not the root of the project but the cwd of the shell. *)
let path_to_lib = "/opt/PaperTerminal/_build/default/lib/dllepd_stubs.so"
let lib_bus = Dl.dlopen ~flags:[Dl.RTLD_LAZY] ~filename:path_to_lib
let funer name params = foreign ~from:lib_bus ~release_runtime_lock:false name params
let vv = void @-> returning void

let init = funer "initBCM" (void @-> returning bool)
let free = funer "freeBCM" vv
let open_bus = funer "openBus" vv
let close_bus = funer "closeBus" vv
let wait_for_bus = funer "waitForBus" vv
let transfer = funer "transfer" (int @-> returning int)

let send value =
  let ret = transfer value in
  wait_for_bus ();
  match ret with
  | 0 -> ()
         (* TODO when are we reading these values? Is this safe to ignore? *)
  | _ -> print_endline ("Error when sending: returned >" ^ string_of_int ret ^ "<")

let recv () = transfer 0x00
