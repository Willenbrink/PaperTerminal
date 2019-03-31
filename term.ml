(*
       0x00005413   TIOCGWINSZ       struct winsize *
       0x00005414   TIOCSWINSZ       const struct winsize *
   *)

open Ctypes
open Foreign

let nth_opt list i =
  try
    Some (List.nth list i)
  with _ -> None

let ioctl = Dl.dlopen ~flags:[Dl.RTLD_LAZY] ~filename:("/home/pi/PaperTerminal/ioctl.so")
let funer name params = foreign ~from:ioctl ~release_runtime_lock:false name params

let set_term_dim x y = funer "set_screen_dimensions" (int @-> int @-> returning void) x y
let get_term_dim () =
  let () = funer "get_screen_dimensions" (void @-> returning void) () in
  funer "getx" (void @-> returning int) (), funer "gety" (void @-> returning int) ()

let read_vcs () =
  let fd = open_in "/dev/vcs" in
  let text = input_line fd in
  let () = close_in fd in
  text

let main () =
  EPD.init () |> ignore;
  EPD.clear EPD.bg;
  EPD.display_buffer (0,0) (800,600) EPD.White;
  let rec loop prevlines =
    let text = read_vcs () in
    let lines =
      let rec worker curr =
        let len = min (String.length text - 10 - curr) 82 in
        if len <= 0 then []
        else String.sub text curr len :: worker (curr+len)
      in
      worker 0
    in
    let update_line i s =
      print_endline s;
      EPD.draw_text (0, i * 16) s;
      EPD.display_buffer (0, i * 16) (800, i * 16 + 16) EPD.Fast
    in
    List.iteri (fun i s -> match nth_opt prevlines i with
        | None -> update_line i s
        | Some line -> match s <> line with
          | true -> update_line i s
          | false -> ()
      ) lines;
    flush_all ();
    Unix.sleep 1;
    loop lines
  in
  loop []

;;
main ()
(*
;;

set_term_dim 10 10;
let x,y = get_term_dim () in
Printf.printf "%d - %d" x y
   *)
