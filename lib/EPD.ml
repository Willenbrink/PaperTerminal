open Ctypes
open Foreign
open Controller

let libIT = Dl.dlopen ~flags:[Dl.RTLD_LAZY] ~filename:("/opt/IT8951/eink-display/IT8951")

let funer name params = foreign ~from:libIT ~release_runtime_lock:false name params
let vv = void @-> returning void

type point = int * int
type area = point * point
type image = int carray
type mode = White | Unknown | Slow | Medium | Fast
let mode_to_int = function White -> 0 | Unknown -> 1 | Slow -> 2 | Medium -> 3 | Fast -> 4

let fg = 0x00
let bg = 0xFF

let check (x,y) = assert (x >= 0 && y >= 0 && x < width () && y < height ())

let free () = Controller.free ()

let plot (x,y) =
  check (x,y);
  plot fg (x,y)

let clear (c : int) = clear_color c

let get_screen () = (0, 0, width (), height ())

(* TODO unused
let load_image img ((x1,y1),(x2,y2) as a) =
  check_area a;
  load_image !img (x1,y1,x2, y2)
*)

let display area mode =
  display area (mode_to_int mode)

let display_buffer area mode =
  display_buffer area (mode_to_int mode)

let display_all mode = display (get_screen ()) mode

let display_buffer_all mode = display_buffer (get_screen ()) mode

let draw_char ?(fg=fg) ?(bg=bg) (x,y as p) c =
  (*TODO check area instead of point <- calculate area of char beforehand*)
  check p;
  put_char p c fg bg

let draw_text ?(fg=fg) ?(bg=bg) (x,y as p) str =
  (*TODO check area instead of point <- calculate area of char beforehand*)
  check p;
  put_text p str fg bg

let rgb r g b = (r+g+b)/3

let random int = Random.int int

let point () = random (width () -1), random (height () -1)

let char () = random 255 |> Char.chr

(* Returns true on failure TODO is this correct? *)
let () = Controller.init ()

