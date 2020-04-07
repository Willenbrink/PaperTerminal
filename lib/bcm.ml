open Ctypes
open Foreign

let libIT = Dl.dlopen ~flags:[Dl.RTLD_LAZY] ~filename:("/opt/IT8951/eink-display/IT8951")
let funer name params = foreign ~from:libIT ~release_runtime_lock:false name params
let vv = void @-> returning void

let init = funer "initBCM" (void @-> returning bool)
let free = funer "freeBCM" vv
let open_bus = funer "openBus" vv
let close_bus = funer "closeBus" vv
let wait_for_bus = funer "waitForBus" vv
let transfer = funer "transfer" (int @-> returning int)
let send value =
  let ret = transfer value in
  match ret with
  | 0 -> ()
  | _ -> print_endline ("Error when sending: returned >" ^ string_of_int ret ^ "<")
