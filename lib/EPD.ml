open Controller

(* Fourth layer: Provide basic drawing primitives and interact with the buffer *)

(* TODO adjust range depending on bpp_mode*)
let fg = 0x00
let bg = 0xFF

let buffer () = State.get_buffer ()

let width () = Array.length (buffer ()).(0)
let height () = Array.length (buffer ())
let get_screen () = (0, 0, width (), height ())

(* TODO unused
let load_image img ((x1,y1),(x2,y2) as a) =
  check_area a;
  load_image !img (x1,y1,x2, y2)
*)

let display_all mode = display (get_screen ()) mode

let display_buffer_all mode = display_buffer (get_screen ()) mode

let rgb r g b = (r+g+b)/3

let random int = Random.int int

let point () = random (height () -1), random (width () -1)

let char () = random 255 |> Char.chr


let plot (y,x) =
  try
    (buffer ()).(y).(x) <- fg
  with _ -> ()

let line (p1, p2) =
  (* Printf.printf "Plotting (y:%i,x:%i) (y:%i,x:%i)\n" y1 x1 y2 x2; *)
  Bresenham.draw_line ~f:(fun x y -> plot (x,y)) ~p0:p1 ~p1:p2

let draw_points amount =
  List.init amount (fun _ -> point ())
  |> List.iter plot

let draw_lines amount =
  List.init amount (fun _ -> (point (), point ()))
  |> List.iter line

let repeat f =
  while true do
    f ()
  done

let test_points () =
  draw_points 1000;
  display_buffer_all `Fast

let test_lines () =
  draw_lines 100;
  display_buffer_all `Fast

let _ =
  Controller.init
