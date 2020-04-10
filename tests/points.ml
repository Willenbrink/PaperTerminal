open Epd
open EPD

let () =
  Controller.init ();
  for _ = 0 to 19 do
    test_points ()
  done;
  Controller.free ()
