open Epd

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


let rec loop () =
  let text =
    Bigarray.reshape_2
      (Term.read_vcs () |> Bigarray.genarray_of_array1)
      26 82
  in
  let update_char (x,y) =
    text.{y,x}
    |> ignore
    (* TODO
    |> EPD.draw_char (x * 8, y * 16)
       *)
  in
  let display dirty_areas =
    List.iter (fun ((x1,y1),(x2,y2)) ->
        Controller.display ((x1*8,y1*8,x2*8+8,y2*16+16)) `Fast) dirty_areas
  in

  (*
  let chars_changed = Matrix.diff text prevText in
  List.iter update_char chars_changed;
  let dirty_areas = Merge.f chars_changed in
  display dirty_areas;
     *)
  ()

let () =
  Controller.init ();
  Term.set_term_dim 37 100;
  loop ()
