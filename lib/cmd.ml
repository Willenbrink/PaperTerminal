(* TODO rename to something clearer *)

open IT8951

let sys_reg_base = 0x0000
let mcsr_base_addr = 0x0200
let mcsr = mcsr_base_addr
let lisar = mcsr_base_addr + 0x8
let i80cpcr = sys_reg_base + 0x04
let display_reg_base = 0x1000
let lutafsr = display_reg_base + 0x224

let vcom = 1500 (* -1.53V = 1530 = 0x5FA *)

type bpp = Bpp2 | Bpp3 | Bpp4 | Bpp8
type rotation = Down | Right | Up | Left
type image = { big_endian : bool; bpp : bpp; rotation : rotation; bitmap : int array array ref; dest : int }
type device_info = { width : int; height : int; address : int; fwversion : string; lutversion : string }

let read_reg address =
  write_cmd_args Register_read [address];
  read_data_single ()

let write_reg address value =
  write_cmd_args Register_write [address; value]

let get_vcom () =
  write_cmd_args VCOM [0];
  read_data_single ()

let set_vcom vcom =
  write_cmd_args VCOM [1; vcom]

let init () =
  let success : bool = IT8951.init () in
  write_reg i80cpcr 0x0001;
  set_vcom vcom;
  success

let free () =
  IT8951.free ()

let print_device_info {width; height; address; fwversion; lutversion} =
  Printf.(
    printf "****** IT8951 ******\n";
    printf "Width: %i\tHeight: %i\n" width height;
    printf "Buffer Address: 0x%x\n" address;
    printf "FW Version: %s\nLUT Version: %s\n" fwversion lutversion
  )

let get_device_info () =
  let rec aux_swapper xs = match xs with
    | x::y::xs -> x::y::aux_swapper xs
    | xs -> xs
  in
  write_cmd Dev_info;
  let result = read_data 20 in
  List.iter (fun i -> print_endline @@ string_of_int i) result;
  (* TODO assuming a read of 16 bit each *)
  match result with
  | (width::height::addressL::addressH::rest) ->
    let rest = List.mapi (fun i c -> [(i,c lsr 8);(i,c land 0xFF)]) rest |> List.concat in
    let fwversion =
      List.filter_map (fun (i,c) -> if i < 8 then Some (char_of_int c) else None) rest
      |> aux_swapper
      |> List.to_seq
      |> String.of_seq
    in
    let lutversion =
      List.filter_map (fun (i,c) -> if i >= 8 then Some (char_of_int c) else None) rest
      |> aux_swapper
      |> List.to_seq
      |> String.of_seq
    in
    let info = { width; height; address = (addressH lsl 16) lor addressL; fwversion; lutversion} in
    print_device_info info;
    info
  | _ -> failwith "Error with info"

(* TODO not included:
systemRun
standBy
initSleep
*)

let burst_read_start address length =
  let args =
    [address land 0xFFFF;
     (address lsr 16) land 0xFFFF;
     length land 0xFFFF;
     (length lsr 16) land 0xFFFF]
  in
  write_cmd_args Burst_read_t args;
  write_cmd Burst_read_s

let burst_write_start address length =
  let args =
    [address land 0xFFFF;
     (address lsr 16) land 0xFFFF;
     length land 0xFFFF;
     (length lsr 16) land 0xFFFF]
  in
  write_cmd_args Burst_write args

let burst_end () =
  write_cmd Burst_end

let burst_write address content =
  burst_write_start address (List.length content);
  write_data content;
  burst_end ()

let burst_read address amount =
  burst_read_start address amount;
  let result = read_data amount in
  burst_end ();
  result

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

let load_img_start {big_endian; bpp; rotation; _} =
  let arg =
    ((int_of_bool big_endian) lsl 8)
    lor ((int_of_bpp bpp) lsl 4)
    lor (int_of_rot rotation)
  in
  write_cmd_args Load_image [arg]

let load_img_area_start {big_endian; bpp; rotation; _} (x,y,w,h) =
  let args =
    [((int_of_bool big_endian) lsl 8)
     lor ((int_of_bpp bpp) lsl 4)
     lor (int_of_rot rotation);
     x;y;w;h]
  in
  write_cmd_args Load_image_area args

let load_img_end () =
  write_cmd Load_image_end

let set_image_buffer_base_addr address =
  let wordH = (address lsr 16) land 0xFFFF in
  let wordL = address land 0xFFFF in
  write_reg (lisar + 2) wordH;
  write_reg lisar wordL

let rec wait_for_display_ready () =
  match read_reg lutafsr with
  | 0 -> ()
  | _ -> wait_for_display_ready ()

let load_image ({dest; bitmap; _} as img) area =
  let helper acc x = match acc with
    | None -> Some x
    | Some y ->
      let value = (y lsl 8) lor x in
      write_data [value];
      None
  in
  set_image_buffer_base_addr dest;
  load_img_area_start img area;
  Matrix.get_rows !bitmap
  |> List.concat
  |> List.fold_left helper None
  |> ignore;
  load_img_end ()

let display_area (x,y,w,h) display_mode =
  wait_for_display_ready ();
  write_cmd_args DPY_area [x; y; w; h; display_mode]

(* TODO unused:
   let display_area_1bpp (x,y,w,h) 
   display_area_buffer
*)
