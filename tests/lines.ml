open Epd
open EPD

let () =
  Controller.init ();
  for _ = 0 to 3 do
    test_lines ()
  done
