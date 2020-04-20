open Ctypes
open Foreign

(* First layer: Interact with the bus and transfer bytes *)

(* TODO use monads to ensure safe usage:
 * init returns some abstract type (init-type) which is used
 * to create another abstract type (bus-type).
 * This is then passed through all the sends/recvs and
 * finally turned into another init-type with close_bus.
 * This would ensure that all illegal operations are forbidden
 * Additionally it would result in a significant(?) overhead when using this library
 *)

(* See the dune file in ./lib.
   TODO Currently hardcoded as cwd is not the root of the project but the cwd of the shell. *)
let path_to_lib = "/opt/PaperTerminal/_build/default/lib/dllepd_stubs.so"
let lib_bus = Dl.dlopen ~flags:[Dl.RTLD_LAZY] ~filename:path_to_lib
let funer name params = foreign ~from:lib_bus ~release_runtime_lock:false name params
let vv = void @-> returning void

let init () =
  funer "initBCM" (void @-> returning bool) ()
  |> function
  | false -> failwith "Bus-initialisation failed. Insufficient permissions?"
  | true -> ()

let free = funer "freeBCM" vv

let open_bus () =
  funer "openBus" vv ()

let close_bus () =
  funer "closeBus" vv ()

let wait_for_bus x =
  funer "waitForBus" vv ();
  x

let transfer value =
  funer "transfer" (int @-> returning int) value

let send value =
  if (value land 0x0000) <> 0
  then failwith "Only the rightmost 16-bit are sent";
  transfer value
  |> wait_for_bus
  |> function
  | 0 -> ()
  | ret ->
    Printf.sprintf "Sending value %i returned %i instead of 0" value ret
    |> failwith

let recv () =
  transfer 0x00
