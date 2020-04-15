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

