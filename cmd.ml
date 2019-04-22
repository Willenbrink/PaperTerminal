(* TODO rename to something clearer *)

open It8951

let sys_reg_base = 0x0000
let i80cpcr = sys_reg_base + 0x04
let vcom = 1500 (* -1.53V = 1530 = 0x5FA *)

type bpp = Bpp2 | Bpp3 | Bpp4 | Bpp8
type rotation = Down | Right | Up | Left
type image = { big_endian : bool; bpp : bpp; rotation : rotation }

let read_reg address = write_cmd_args Register_read [adress]
let write_reg address value = write_cmd_args Register_write [adress; value]

let get_vcom =
  write_cmd_args VCOM [0];
  read_data ()

let set_vcom vcom = write_cmd_args VCOM [1; vcom]

let init () =
  let failure : bool = It8951.init () in
  write_reg i80cpcr 0x0001;
  set_vcom vcom;
  failure

let free () = It8951.free ()

let burst_read_start address length =
  write_cmd_args Burst_read_t
    [address land 0xFFFF; (address lsr 16) land 0xFFFF; length land 0xFFFF; (length lsr 16) land 0xFFFF];
  write_cmd Burst_read_s

let burst_write_start address length =
  write_cmd_args Burst_write
    [address land 0xFFFF; (address lsr 16) land 0xFFFF; length land 0xFFFF; (length lsr 16) land 0xFFFF]

let burst_end () = write_cmd Burst_end

let int_of_bool = function true -> 1 | false -> 0

let int_of_bpp = function
  | Bpp2 -> 0
  | Bpp3 -> 1
  | Bpp4 -> 2
  | Bpp8 -> 3

let int_of_rot = function
  | Down -> 0
  | Right -> 1
  | Up -> 2
  | Left -> 3

let load_img_start {big_endian; bpp; rotation} =
  let arg = ((int_of_bool big_endian) lsl 8) lor ((int_of_bpp bpp) lsl 4) lor (int_of_rot rotation) in
  write_cmd_args Load_image [arg]
