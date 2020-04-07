type 'a t = 'a array array

(* In docs: x then y but we want to simplify access to rows -> first rows then cols
   The get_row basically reads one value along the x dimension of the array
   which corresponds to a column in regards to the memory layout but we want rows instead of columns.
   TL;DR: Rows and columns are swapped
*)
let create cols rows value : 'a t =
  Array.make_matrix rows cols value

let set t c r v =
  let rows = Array.length t in
  let cols = Array.length t.(0) in
  if r < 0 || c < 0 || r >= rows || c >= cols
  then Printf.printf "Index OOB: %i:%i not within 0:0-%i:%i\n" r c rows cols
  else t.(r).(c) <- v

let get t c r = t.(r).(c)
let get_row t r = t.(r) |> Array.to_list
let get_rows t =
  Array.map (Array.to_list) t |> Array.to_list

let mapi_inplace f t =
  for i = 0 to Array.length t do
    for j = 0 to Array.length t.(0) do
      set t i j (f i j)
    done
  done

let mapi f t =
  Array.mapi (fun row_index row -> Array.mapi (fun col_index color -> f col_index row_index color) row) t

let iteri f t =
  Array.iteri (fun row_index row -> Array.iteri (fun col_index char -> f (col_index,row_index) char) row) t

let fold f_row f_col a b (t : 'a t) = (* Fold row by row, left to right*)
  let fold_col = Array.fold_left (fun a char -> f_col a char) in
  let fold_row = Array.fold_left (fun a row -> f_row a (fold_col b row)) in
  fold_row a t

let diff t1 t2 =
  let t_index = mapi (fun x y c -> (x,y,c)) t1 in
  fold (@) (fun a (x,y,c) -> if get t2 x y = c then a else (x,y)::a) [] [] t_index
