open Ctypes
open Foreign
open Epd

let nth_opt list i =
  try
    Some (List.nth list i)
  with _ -> None

let ioctl = Dl.dlopen ~flags:[Dl.RTLD_LAZY] ~filename:(Unix.getcwd () ^ "/bin/ioctl.so")
let funer name params = foreign ~from:ioctl ~release_runtime_lock:false name params

let set_term_dim x y = funer "set_screen_dimensions" (int @-> int @-> returning void) x y
let get_term_dim () =
  let () = funer "get_screen_dimensions" (void @-> returning void) () in
  funer "getx" (void @-> returning int) (), funer "gety" (void @-> returning int) ()

module type Draw =
sig
  val f : (int * int) list -> ((int * int) * (int * int)) list
end

module BoundingBox : Draw =
struct
  let f points =
    List.fold_left
      (fun area (x,y) -> match area with
         | None -> Some ((x,y),(x,y))
         | Some ((lx,ly),(hx,hy)) -> Some ((min lx x, min ly y),(max hx x, max hy y)))
      None
      points
    |> function None -> [] | Some area -> [area]
end

module Naive : Draw =
struct
  let f points = List.rev_map (fun p -> p,p) points
end

module Merge : Draw = (* Merge neighbouring dirty areas to one, continue until no further merge is possible. Could be improved by allowing non-dirty areas in the merge*)
struct
  let f points =
    let areas = List.map (fun p -> p,p) points in
    let rec worker acc xs = match xs with
      | [] -> acc
      | x::xs ->
        let area_new,xs,changed =
          List.fold_left
            (fun ((((x1,y1),(x2,y2)) as area), xs, changed) ((x1',y1'),(x2',y2')) ->
               let margin = 2 in
               let within_margin (x1,y1) (x2,y2) = x2 <= x1 + margin && x2 >= x1 - margin && y2 <= y1 + margin && y2 >= y1 - margin in
               let upper_left = within_margin (x1,y1) (x1',y1') in
               let lower_right = within_margin (x2,y2) (x2',y2') in

               let inside = x1' >= x1 && y1' >= y1 && x2' <= x2 && y2' <= y2 in
               if inside then area, xs, changed
               else
                 let x1,y1 = if upper_left then min x1 x1', min y1 y1' else x1,y1 in
                 let x2,y2 = if lower_right then max x2 x2', max y2 y2' else x2,y2 in
                 let area' = (x1,y1),(x2,y2) in
                 if area <> area' then area', xs, true
                 else area', (((x1',y1'),(x2',y2'))::xs), changed
            ) (x,[],false) xs
        in
        if changed
        then worker acc (area_new::xs)
        else worker (area_new::acc) xs

    in
    worker [] areas

end


let read_vcs () =
  let fd = open_in "/dev/vcsa1" in
  let rows = input_byte fd in
  let cols = input_byte fd in
  let cursor_x = input_byte fd in
  let cursor_y = input_byte fd in
  (*TODO resize instead of creating it completely new*)
  let text,mods =
    let text = Matrix.create cols rows ' ' in
    let modifiers = Matrix.create cols rows 0 in
    let rec worker x y =
      try
        if y >= rows then text,modifiers
        else if x >= cols then worker 0 (y+1)
        else
          let byte1,byte2 = input_byte fd, input_byte fd in
          Matrix.set text x y (char_of_int byte2);
          Matrix.set modifiers x y byte1;
          worker (x+1) y
      (* This should never occur! We've just read the size and expect it to be true!*)
      with End_of_file -> raise End_of_file
    in
    worker 0 0
  in

  Matrix.set text cursor_x cursor_y (char_of_int 0x7F);

  ignore mods;
  let () = close_in fd in
  text

let init () =
  print_endline "Configuring virtual terminal";
  set_term_dim 37 100;
  print_endline "Clearing buffer";
  EPD.clear EPD.bg;
  print_endline " Updating screen";
  EPD.display_buffer_all EPD.White


let rec loop prevText =
  let text = read_vcs () in
  let update_char (x,y) =
    Matrix.get text x y
    |> EPD.draw_char (x * 8, y * 16)
  in
  let display dirty_areas =
    (*
    List.iter (fun ((x1,y1),(x2,y2)) -> Printf.printf "%i-%i : %i-%i\t" y1 x1 y2 x2) dirty_areas;
    if List.length dirty_areas <> 0 then print_endline "";
    *)
    List.iter (fun ((x1,y1),(x2,y2)) -> EPD.display_buffer ((x1*8,y1*8,x2*8+8,y2*16+16)) EPD.Fast) dirty_areas
  in

  let chars_changed = Matrix.diff text prevText in
  let () = List.iter update_char chars_changed in
  let dirty_areas = Merge.f chars_changed in
  let () = display dirty_areas in
  loop text

let main () =
  init ();
  loop (Matrix.create 100 37 (char_of_int 0))

let () = ()
