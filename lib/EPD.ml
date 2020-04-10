open Controller

(* Fourth layer: Provide basic drawing primitives and interact with the buffer *)

(* TODO adjust range depending on bpp_mode*)
let fg = 0x00
let bg = 0xFF

let buffer () = State.get_buffer ()

let width () = Bigarray.Array2.dim1 (buffer ())
let height () = Bigarray.Array2.dim2 (buffer ())
let get_screen () = (0, 0, width (), height ())

(* TODO unused
let load_image img ((x1,y1),(x2,y2) as a) =
  check_area a;
  load_image !img (x1,y1,x2, y2)
*)

let display_all mode =
  display (get_screen ()) mode

let refresh mode =
  let screen = get_screen () in
  transmit screen;
  display screen mode

let rgb r g b = (r+g+b)/3

let random int = Random.int int

let point () = random (height () -1), random (width () -1)

let char () = random 255 |> Char.chr


let plot (y,x) =
  try
    (buffer ()).{x,y} <- fg
  with _ -> print_endline "Out_of_bounds plot ignored"

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
  draw_points 10000;
  refresh `Fast

let test_lines () =
  draw_lines 100;
  refresh `Medium

let _ =
  Controller.init
