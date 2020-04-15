
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
    | None -> Array.init 9 (fun _ -> Array.make 15 true)
    | Some value -> value
  in
  let x_size = Array.length array.(0) in
  let y_size = Array.length array in
  for j = 0 to y_size - 1 do
    for i = 0 to x_size - 1 do
      if array.(j).(i)
      then
        EPD.color (y_pos + j, x_pos + i) EPD.fg
      else
        EPD.color (y_pos + j, x_pos + i) EPD.bg
    done
  done
