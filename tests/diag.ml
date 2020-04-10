open Epd

let () =
  Controller.init ();
  List.init 600 (fun i -> (i,i))
  |> List.iter (fun (x,y) ->
      EPD.plot (x,y); EPD.plot(600-x,y));
  EPD.refresh `Medium;
  List.init 600 (fun i -> (i,i))
  |> List.iter (fun (x,y) ->
      EPD.plot (x,200+y); EPD.plot(x,800-y));
  EPD.refresh `Medium;
  Controller.free ()

