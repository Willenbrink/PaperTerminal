open Epd

let () =
  Controller.init ();
  List.init 600 (fun i -> (i,i))
  |> List.iter (fun (x,y) ->
      EPD.plot (x,y); EPD.plot(599-x,y));
  EPD.refresh Medium;
  List.init 600 (fun i -> (i,i))
  |> List.iter (fun (x,y) ->
      EPD.plot (x+200,y); EPD.plot(x+200,599-y));
  EPD.refresh Medium;
  EPD.line ((400,0),(400,598));
  EPD.refresh Medium;
  EPD.line ((0,300),(798,300));
  EPD.refresh Medium;
  Controller.free ()

