open Epd

let rec loop () =
  let text =
      Term.read_vcs ()
      |> fun x -> Bigarray.reshape_2 x 26 82
  in
  let update_char (x,y) =
    try
      Text.draw_char text.{y,x} (x * 10, y * 16)
    with
    | _ -> ()
  in
  let display (x,y,w,h) =
    let area = x*8,y*16,x*8+w,y*16+h in
    try
      Controller.transmit area;
      Controller.display area `Fast
    with
      _ -> print_endline "Ignored out-of-bounds transmit"
  in
  (* TODO *)
  ignore display;
  let dim_y = Bigarray.Array2.dim1 text in
  let dim_x = Bigarray.Array2.dim2 text in
  for y = 0 to dim_y - 1 do
    for x = 0 to dim_x - 1 do
      update_char (x,y)
    done
  done;
      (*
  for y = 0 to dim_y - 1 do
    for x = 0 to dim_x - 1 do
      display (x,y,16,16)
      ()
    done
  done;
         *)
  EPD.refresh `Medium;
  loop ()

  (*
  let chars_changed = Matrix.diff text prevText in
  List.iter update_char chars_changed;
  let dirty_areas = Merge.f chars_changed in
  display dirty_areas;
     *)

let () =
  Controller.init ();
  Text.init ();
  (* TODO this function is ineffective. *)
  Term.set_term_dim 60 60;
  loop ()
