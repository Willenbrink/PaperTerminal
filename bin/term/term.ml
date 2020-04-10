open Ctypes
open Foreign

let ioctl = Dl.dlopen ~flags:[Dl.RTLD_LAZY] ~filename:(Unix.getcwd () ^ "/ioctl.so")
let funer name params = foreign ~from:ioctl ~release_runtime_lock:false name params

let set_term_dim x y =
  funer "set_screen_dimensions" (int @-> int @-> returning void) x y

let get_term_dim () =
  let () = funer "get_screen_dimensions" (void @-> returning void) () in
  funer "getx" (void @-> returning int) (), funer "gety" (void @-> returning int) ()

let read_vcs () =
  let fd = open_in "/dev/vcsa1" in
  (* TODO find a nicer way *)
  let [rows; cols; cursor_x; cursor_y] =
    match List.init 4 (fun _ -> input_byte fd) with
    | [a;b;c;d] -> a
  Printf.printf "TTY Info:\nRows: %i\nCols: %i\n" rows cols;

  (* TODO resize instead of creating it completely new *)
  let text =
    Bigarray.Array1.create
      Bigarray.Char Bigarray.C_layout
      (cols*rows)
  in
  let modifiers =
    Bigarray.Array1.create
      Bigarray.Int8_unsigned Bigarray.C_layout
      (cols*rows)
  in
  for pos = 0 to cols*rows - 1 do
    modifiers.{pos} <- input_byte fd;
    text.{pos} <- (char_of_int (input_byte fd));
  done;

  text.{cursor_y*cols + cursor_x} <- (char_of_int 0x7F);

  ignore modifiers;
  close_in fd;
  text
