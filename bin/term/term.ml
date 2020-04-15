open Ctypes
open Foreign

let ioctl = Dl.dlopen ~flags:[Dl.RTLD_LAZY] ~filename:("/opt/PaperTerminal/bin/term/ioctl.so")
let funer name params = foreign ~from:ioctl ~release_runtime_lock:false name params

let set_term_dim x y =
  funer "set_screen_dimensions" (int @-> int @-> returning void) x y

let get_term_dim () =
  funer "get_screen_dimensions" (void @-> returning void) ();
  funer "getx" (void @-> returning int) (), funer "gety" (void @-> returning int) ()

let read_vcs () =
  let fd = open_in "/dev/vcsa1" in
  let byte () = input_byte fd in
  (* TODO find a nicer way *)
  let dimy = byte () in
  let dimx = byte () in
  let cursor_x = byte () in
  let cursor_y = byte () in
  Printf.printf "TTY Info:\nDim X: %i\nDim Y: %i\n" dimx dimy;
  let size = dimx * dimy in


  (* TODO resize instead of creating it completely new *)
  let text =
    Bigarray.Array1.create
      Bigarray.Char Bigarray.C_layout
      size
  in
  let modifiers =
    Bigarray.Array1.create
      Bigarray.Int8_unsigned Bigarray.C_layout
      size
  in
  for pos = 0 to size - 1 do
    text.{pos} <- (char_of_int (input_byte fd));
    modifiers.{pos} <- input_byte fd;
  done;

  text.{cursor_y*dimx + cursor_x} <- (char_of_int 0x7F);

  ignore modifiers;
  close_in fd;
  text
  |> Bigarray.genarray_of_array1
