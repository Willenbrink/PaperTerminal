open Command
open State

(* Third layer: Control the IT8951 by sending commands *)

let get_vcom () =
  write_cmd_args `VCOM [0];
  read_datum ()

let set_vcom vcom =
  write_cmd_args `VCOM [1; vcom]

let print_device_info {width; height; address; fwversion; lutversion} =
  Printf.(
    printf "IT8951 Device Info:\n";
    printf "Width: %i\nHeight: %i\nAddress: 0x%x\n" width height address;
    printf "FW Version: %s\nLUT Version: %s\n" fwversion lutversion
  )

let query_device_info () =
  write_cmd `Dev_info;
  match read_data 20 with
  | (width::height::addressL::addressH::rest) ->
    let rest =
      (* TODO Splits every read 16-bit value into two chars *)
      List.mapi (fun i c -> [(i,c lsr 8);(i,c land 0xFF)]) rest
      |> List.concat
    in
    let fwversion =
      List.filter_map (fun (i,c) -> if i < 8 then Some (char_of_int c) else None) rest
      |> List.to_seq |> String.of_seq (* TODO poor mans List.to_string/String.from_list *)
    in
    let lutversion =
      List.filter_map (fun (i,c) -> if i >= 8 then Some (char_of_int c) else None) rest
      |> List.to_seq |> String.of_seq (* TODO poor mans List.to_string/String.from_list *)
    in
    let info = { width; height; address = (addressH lsl 16) lor addressL; fwversion; lutversion} in
    print_device_info info;
    info
  | _ -> failwith "Error while getting device_info: read data too short."

(* TODO not included:
systemRun
standBy
initSleep
*)

(* TODO move somewhere appropriate *)
let split_32 value =
  assert (value land 0xFFFF0000 == 0);
  (value land 0xFFFF, (value lsr 16) land 0xFFFF)

let flatten_list xs = List.map (fun (x,y) -> [x;y]) xs |> List.concat

let burst_write address content =
  let amount = List.length content in
  let args =
    [split_32 address; split_32 amount]
    |> flatten_list
  in
  write_cmd_args `Burst_write args;
  write_data content;
  write_cmd `Burst_end

let burst_read address amount =
  let args =
    [split_32 address; split_32 amount]
    |> flatten_list
  in
  write_cmd_args `Burst_read_trigger args;
  write_cmd `Burst_read_start;
  let result = read_data amount in
  write_cmd `Burst_end;
  result

let int_of_bool = function true -> 1 | false -> 0

let int_of_bpp = function
 (* TODO packing *)
  | `Bpp2 -> 0 (* Packing: 1100_1100 *)
  | `Bpp3 -> 1 (* Packing: 1110_1110 *)
  | `Bpp4 -> 2 (* Packing: 1111_1111 *)
  | `Bpp8 -> 3 (* Packing: 1111_1111 *)

let int_of_rot = function
  (* TODO Does the memory layout of this impact performance or anything? *)
  | `Down -> 0
  | `Right -> 1
  | `Up -> 2
  | `Left -> 3

let set_image_buffer_base_addr address =
  let wordH = (address lsr 16) land 0xFFFF in
  let wordL = address land 0xFFFF in
  write_reg `LISAR2 wordH;
  write_reg `LISAR wordL

let load_img_start image_info =
  write_cmd_args `Load_image [image_info]

let load_img_area_start image_info (x,y,w,h) =
  let args = image_info :: [x;y;w;h] in
  write_cmd_args `Load_image_area args

let load_img_end () =
  write_cmd `Load_image_end

let load_image image_info area =
  (* TODO figure out something nicer than this *)
  let helper acc x = match acc with
    | None -> Some x
    | Some y ->
      let value = (y lsl 8) lor x in
      write_data [value];
      None
  in
  (* Do not set the address repeatedly, only change when we use a second buffer in memory.
   * Can perhaps be used for some cool effects like moving the address 800*20 bytes along
   * to easily scroll on the screen without retransmitting the whole image. TODO
   *)
  (State.get_dev_info ()).address
  |> set_image_buffer_base_addr;
  load_img_area_start image_info area;
  print_endline "Starting buffer transfer";
  State.get_buffer ()
  |> Matrix.get_rows
  |> List.concat
  |> List.fold_left helper None
  |> ignore;
  print_endline "Ending buffer transfer";
  load_img_end ()

let rec wait_for_display_ready () =
  match read_reg `LUTAFSR with
  | 0 -> ()
  | _ -> wait_for_display_ready ()

let display_area (x,y,w,h) display_mode =
  wait_for_display_ready ();
  write_cmd_args `DPY_area [x; y; w; h; display_mode]

(* TODO unused:
   let display_area_1bpp (x,y,w,h) 
   display_area_buffer
*)

let validate_area (x,y,w,h) =
  (* Assert the area is within bounds:
   * Check first upper left corner and after rounding up the bottom right corner.
   * Prevents w = -1 from succeeding.
   *)
  assert (w >= 0 && h >= 0);
  (* TODO always draw 2 pixels, therefore the displayed area must be rounded
   * This is only relevant in the x direction.
   *)
  let w = w + w mod 2 in
  assert (x + w <= (get_dev_info ()).width);
  assert (y + h <= (get_dev_info ()).height);
  (x,y,w,h)

let get_image_info big_endian bpp rotation =
  ((int_of_bool big_endian) lsl 8)
  lor ((int_of_bpp bpp) lsl 4)
  lor (int_of_rot rotation)

let transmit_image area =
  let area = validate_area area in
  let image_info = get_image_info false `Bpp8 `Down in
  load_image image_info area

let display area mode =
  let area = validate_area area in
  display_area area mode

let display_buffer area mode =
  let area = validate_area area in
  transmit_image area;
  display_area area mode

let rgb (r,g,b) =
  (r land 0xFF + g land 0xFF + b land 0xFF) / 3

let clear_color (c : int) =
  Matrix.mapi_inplace (fun _ _ -> c) (get_buffer ())
  |> ignore

let width () = (get_dev_info ()).width

let height () = (get_dev_info ()).height

let plot (c : int) (x,y) =
  (get_buffer ()).(x).(y) <- c

let put_char (x,y) char fg bg =
  (* TODO simple diagonal *)
  plot fg (x,y);
  plot bg (x+1,y);
  plot bg (x,y+1);
  plot fg (x+1,y+1)

let put_text (x,y) str fg bg =
  String.to_seq str
  |> List.of_seq
  |> List.iteri (fun i char -> put_char (x + i*4,y) char fg bg)

let init () =
  (* Initialise bus *)
  begin
    if not (Bus.init ())
    then failwith "Bus initialisation failed, insufficient permissions?"
  end;

  (* Initialise board *)
  write_reg `I80CPCR 0x0001;
  let vcom = 1500 in (* -1.53V = 1530 = 0x5FA *)
  set_vcom vcom;

  (* Initialise state used by other functions *)
  let dev_info = query_device_info () in
  State.set_dev_info dev_info;
  State.set_buffer @@ Matrix.create dev_info.width dev_info.height 0xFF;

  (* Display white screen *)
  display_buffer (0, 0, dev_info.width, dev_info.height) 0

let free () =
  Bus.free ()
