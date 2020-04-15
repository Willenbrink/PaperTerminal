open Epd

let _ =
  Controller.init ();
  Text.init ();
  EPD.line ((98,10),(98,590));
  EPD.line ((115,10),(115,590));
  EPD.refresh `Medium;
  List.init 58 (fun i -> ('G',i*10+10,100))
  |> List.iter (fun (c,x,y) -> Text.draw_char c (x,y); EPD.refresh `Fast)
