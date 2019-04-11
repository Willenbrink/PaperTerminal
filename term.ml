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

module Matrix = struct
  type 'a t = 'a array array

  let create cols rows value : 'a t =
    (* In docs: x then y but we want to access each row all at once -> first rows then cols
       The getRow basically reads one value along the x dimension of the array , which corresponds to a column
       but we want rows instead of columns*)
    Array.make_matrix rows cols value
  let set t c r v =
    let rows = Array.length t in
    let cols = Array.length t.(0) in
    if r < 0 || c < 0 || r >= rows || c >= cols then Printf.printf "Accessing %i:%i with max %i:%i\n" r c rows cols
    else
    t.(r).(c) <- v
  let get t c r = t.(r).(c)
  let getRow t r = t.(r) |> Array.to_list
  let getRows t =
    Array.map (Array.to_list) t |> Array.to_list

  let mapi f t =
    Array.mapi (fun row_index row -> Array.mapi (fun col_index char -> f col_index row_index char) row) t

  let iteri f t =
    Array.iteri (fun row_index row -> Array.iteri (fun col_index char -> f col_index row_index char) row) t

  let fold f_row f_col a b (t : 'a t) = (* Fold row by row, left to right*)
    let fold_col = Array.fold_left (fun a char -> f_col a char) in
    let fold_row = Array.fold_left (fun a row -> f_row a (fold_col b row)) in
    fold_row a t

  let diff t1 t2 =
    let t_index = mapi (fun x y c -> (x,y,c)) t1 in
    fold (@) (fun a (x,y,c) -> if get t2 x y = c then a else (x,y)::a) [] [] t_index
end

let read_vcs () =
  let fd = open_in "/dev/vcsa1" in
  let rows = input_byte fd in
  let cols = input_byte fd in
  let x = input_byte fd in
  let y = input_byte fd in
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

  (* x,y is the cursor position *)
  Matrix.set text x y (char_of_int 0x7F);

  ignore mods;
  let () = close_in fd in
  text

let main () =
  set_term_dim 37 100;
  EPD.init () |> ignore;
  EPD.clear EPD.bg;
  EPD.display_buffer_all EPD.White;
  let rec loop prevText =
    let text = read_vcs () in
    let lines : string list =
      Matrix.getRows text
      |> List.map (List.map (fun c -> String.make 1 c))
      |> List.map (String.concat "")
    in

    let chars_changed = Matrix.diff text prevText in

    let update_char (x,y) =
      Matrix.get text x y
      |> EPD.draw_char (x * 8, y * 16)
    in
    let () = List.iter update_char chars_changed in
    let dirty_area =
      List.fold_left
        (fun area (x,y) -> match area with
           | None -> Some ((x,y),(x,y))
           | Some ((lx,ly),(hx,hy)) -> Some ((min lx x, min ly y),(max hx x, max hy y)))
        None
        chars_changed
    |> function None -> None | Some ((lx,ly),(hx,hy)) -> Some ((lx * 8, ly * 16),((hx+1) * 8, (hy+1) * 16))
    in
    (match dirty_area with
    | None -> ()
    | Some dirty_area -> EPD.display_buffer dirty_area EPD.Fast);

    loop text
  in
  loop (Matrix.create 100 37 (char_of_int 0))

let () = main ()
