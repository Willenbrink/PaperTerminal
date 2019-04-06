(*
 * E-Paper Display Emulator
 * Use as replacement while developing on the laptop
 *)
open Graphics

type mode = White | Slow | Medium | Fast

let bg = black
let fg = white

let init () = open_graph ""

let clear _ = clear_graph ()

let display_buffer (x1,y1) (x2,y2) _ =
  let y1 = size_y () - y1 in
  let y2 = size_y () - y2 in
  moveto x1 y1;
  lineto x1 y2;
  lineto x2 y2;
  lineto x2 y1;
  lineto x1 y1

let display_buffer_all _ =
  display_buffer (0,0) (size_x (), size_y ()) ()

let draw_text (x,y) s =
  let y = size_y () - y in
  moveto (x+8) (y - 16);
  draw_string s
