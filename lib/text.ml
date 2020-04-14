
let font : Pcf.t option ref = ref None

let init ?(fontfile="/opt/pcf-parser/9x15.pcf") () =
  match Pcf.of_file fontfile with
  | None ->
    Printf.sprintf "No font found at: %s" (fontfile)
    |> failwith
  | Some f ->
    Printf.eprintf "Text-module:\nFontfile: %s\nNumber of glyphs: %i\n"
      fontfile (Pcf.Glyph.number f);
    font := Some f

let get_font () = match !font with
  | None -> failwith "No font loaded"
  | Some f -> f

let draw_char c (x_pos, y_pos) =
  let array =
    c
    |> int_of_char
    |> Pcf.Encoding.of_int
    |> Pcf.Glyph.get_bitmap (get_font ())
  in
  let array = match array with
    | None -> failwith "Char not present"
    | Some value -> value
  in
  let x_size = Array.length array.(0) in
  let y_size = Array.length array in
  let buffer = State.create_buffer x_size y_size in
  Printf.printf "Y: %i; X: %i" y_pos x_pos;
  flush_all ();
  for j = 0 to y_size - 1 do
    for i = 0 to x_size - 1 do
      if array.(j).(i)
      then
        EPD.plot (y_pos + j, x_pos + i)
    done
  done;
  (*
  Bigarray.Array2.blit buffer (State.get_buffer ());
  EPD.get_screen ()
  |> Controller.transmit;
     *)
  Controller.transmit (y_pos, x_pos, 20, 20);
  Controller.display (y_pos, x_pos, 20, 20) `Unknown
