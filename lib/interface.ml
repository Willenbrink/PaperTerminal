open Cmd
open State

let validate_area (x,y,w,h) =
  (* Assert the area is within bounds. Check first upper left corner and after rounding up the bottom right corner. Prevents w = -1 from succeeding *)
  assert (w >= 0 && h >= 0);
    (* TODO always draw 2 pixels, therefore the displayed area must be rounded down/up
  This is only relevant in the x direction *)
  let w = w + w mod 2 in
  assert (x + w <= (get_dev_info ()).width);
  assert (y + h <= (get_dev_info ()).height);
  (x,y,w,h)

let load_image area =
  let area = validate_area area in
  let image =
    {
      big_endian = false;
      bpp = Bpp8;
      rotation = Down;
      bitmap = ref (get_buffer ());
      dest = (get_dev_info ()).address;
    }
  in
  Cmd.load_image image area

let display area mode =
  let area = validate_area area in
  display_area area mode

let display_buffer area mode =
  let area = validate_area area in
  load_image area;
  display_area area mode

let rgb (r,g,b) =
  (r land 0xFF + g land 0xFF + b land 0xFF) / 3

let clear_color (c : int) =
  Matrix.mapi_inplace (fun _ _ -> c) (get_buffer ())
  |> ignore

let width () = (get_dev_info ()).width

let height () = (get_dev_info ()).height

let plot (c : int) (x,y) =
  (get_buffer ()).(x).(y) <- c

let put_char (x,y) char fg bg =
  plot fg (x,y);
  plot bg (x+1,y);
  plot bg (x,y+1);
  plot fg (x+1,y+1)

let put_text (x,y) str fg bg =
  String.to_seq str
  |> List.of_seq
  |> List.iteri (fun i char -> put_char (x + i*4,y) char fg bg)

let init_graphics () =
  match Cmd.init () with
  | false -> failwith "Init failed, insufficient permissions?"
  | true ->
    let dev_info = Cmd.get_device_info () in
    set_dev_info dev_info;
    let buffer = Matrix.create dev_info.width dev_info.height 0 in
    set_buffer buffer;
    display_buffer (0, 0, dev_info.width, dev_info.height) 0

let free_graphics () =
  Cmd.free ()
