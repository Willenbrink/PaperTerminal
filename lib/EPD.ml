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

let point () = random (width () -1), random (height () -1)

let char () = random 255 |> Char.chr

let color (x,y) color =
  try
    (buffer ()).{x,y} <- color
  with e ->
    Printf.eprintf "OOB plot: X: %i - Y: %i - C: %04x\n" x y color;
    flush stderr;
    raise e

let plot (x,y) =
  color (x,y) fg

let line (p0,p1) =
  Bresenham.draw_line ~f:(fun x y -> plot (x,y)) ~p0 ~p1

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
  refresh Fast

let test_lines () =
  draw_lines 100;
  refresh Fast

let loopfb () =
  let fb = Unix.openfile "/dev/fb0" [Unix.O_RDONLY] 0o664 in
  let loop () =
    Unix.lseek fb 0 Unix.SEEK_SET
    |> ignore;
    let ic = Unix.in_channel_of_descr fb in
    let rec read i () =
      if i <= 0
      then Seq.Nil
      else Seq.Cons ((input_byte ic),(read (i-1)))
    in
    let size = Unix.lseek fb 0 Unix.SEEK_END in
    ignore size;
    let size = 800 * 600 in
    (*
    let bytes = Bytes.create size in
    Unix.read fb bytes 0 size
    |> Printf.printf "Read %i bytes\n";
       *)
    flush_all ();
    let framebuffer =
      read size
      |> Array.of_seq
      |> fun x -> Printf.printf "Read %i bytes\n" (Array.length x); x
      |> Bigarray.Array1.of_array Bigarray.Int8_unsigned Bigarray.c_layout
      |> Bigarray.genarray_of_array1
      |> fun x -> Bigarray.reshape_2 x 800 600
    in
  (*
  let buffer =
    Unix.map_file fb
      Bigarray.int8_unsigned Bigarray.c_layout false
      [|800;600|]
    |> Bigarray.array2_of_genarray
  in
  *)
      Bigarray.Array2.blit framebuffer (State.get_buffer ())
  in
  try
    while true do
      loop ();
      refresh Medium
    done
  with
  | e ->
    print_endline "Failed";
    Unix.close fb;
    raise e
