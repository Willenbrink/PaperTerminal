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

let ioctl = Dl.dlopen ~flags:[Dl.RTLD_LAZY] ~filename:(Unix.getcwd () ^ "/ioctl.so")
let funer name params = foreign ~from:ioctl ~release_runtime_lock:false name params

let set_term_dim x y = funer "set_screen_dimensions" (int @-> int @-> returning void) x y
let get_term_dim () =
  let () = funer "get_screen_dimensions" (void @-> returning void) () in
  funer "getx" (void @-> returning int) (), funer "gety" (void @-> returning int) ()

module Screen = struct
  type 'a t = 'a array array

  let create cols rows value : 'a t = Array.make_matrix cols rows value
  let set t c r v =
    let rows = Array.length t in
    let cols = Array.length t.(0) in
    if r >= rows || c >= cols then Printf.printf "Accessing %i:%i with max %i:%i\n" r c rows cols
    else
    t.(r).(c) <- v
  let get t c r = t.(r).(c)
  let getRow t r = t.(r) |> Array.to_list
  let getRows (t : 'a t) =
    Array.map (Array.to_list) t |> Array.to_list

end

let read_vcs () =
  let fd = open_in "/dev/vcsa1" in
  let rows = input_byte fd in
  let cols = input_byte fd in
  let x = input_byte fd in
  let y = input_byte fd in
  ignore (x + y);
  let text = Screen.create cols rows ' ' in
  let modifiers = Screen.create cols rows 0 in
  let bytes () =
    let rec worker x y =
      try
        let byte1,byte2 = input_byte fd, input_byte fd in
        Screen.set text x y (char_of_int byte1);
        Screen.set modifiers x y byte2;
        if y >= rows then text,modifiers
        else if x >= cols then worker 0 (y+1)
        else worker (x+1) y
      with End_of_file -> text,modifiers
    in
    worker 0 0
  in
  let (text,mods) = bytes () in
  ignore mods;
  let () = close_in fd in
  text

let read_vcs () =
  Screen.create 82 26 'c'

let main () =
  EPD.init () |> ignore;
  EPD.clear EPD.bg;
  EPD.display_buffer_all EPD.White;
  let rec loop prevlines =
    let text = read_vcs () in
    let lines : string list = ["bla";"blub"] in
    let lines : string list =
      Screen.getRows text
      |> List.map (List.map (fun c -> String.make 1 c))
      |> List.map (String.concat "") in
    let linesChanged =
    List.mapi (fun i s -> i,s) lines
    |> List.fold_left (fun a (i,s) ->
                    let line = match nth_opt prevlines i with None -> "" | Some line -> line in
                    if s <> line then a+1 else a) 0
  in

    let update_line i s =
      EPD.draw_text (0, i * 16) s;
      if linesChanged < 3 then EPD.display_buffer ((0, i * 16),(799, i * 16 + 16)) EPD.Fast
    in
    List.iteri (fun i s -> match nth_opt prevlines i with
        | None -> update_line i s
        | Some line -> match s <> line with
          | true -> update_line i s
          | false -> ()
      ) lines;
    flush_all ();
    (if linesChanged >= 3 then EPD.display_buffer_all EPD.Fast);
    (*loop lines*)
    ()
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
