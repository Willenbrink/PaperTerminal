open Ctypes
open Foreign

let vv = void @-> returning void

let init = funer "initBCM" (int @-> returning bool)
let free = funer "freeBCM" vv
let open_bus = funer "openBus" vv
let close_bus = funer "closeBus" vv
let wait_for_bus = funer "waitForBus" vv
let transfer = funer "transfer" (int @-> returning int)
