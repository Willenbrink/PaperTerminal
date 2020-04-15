open Command
open State

(* Third layer: Control the IT8951 by sending commands *)

let int_of_mode = function
  | `White -> 0
  (* TODO what does this mode do? And has this been switched around?
   * Unknown works well whereas fast repeatedly fails to display anything.
   * Fast only works for bpp8 and Unknown is only quick for the other modes.
   * TODO this needs further investigation, because the above sounds improbable.
   *)
  | `Unknown -> 1
  | `Slow -> 2
  | `Medium -> 3
  | `Fast -> 4

(* TODO move somewhere appropriate *)
let split_32 value =
  assert (value land 0x7FFFFFFF00000000 == 0);
  (value land 0xFFFF, (value lsr 16) land 0xFFFF)

let flatten_list xs = List.map (fun (x,y) -> [x;y]) xs |> List.concat

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

let set_image_buffer_base_addr address =
  let wordH = (address lsr 16) land 0xFFFF in
  let wordL = address land 0xFFFF in
  write_reg `LISAR2 wordH;
  write_reg `LISAR wordL

(* TODO unused because transmitting the area too does not affect performance
   let load_img_start image_info =
   write_cmd_args `Load_image [image_info]
   let display_area_buffer =
   *)

(*
 * aggregate combines the values of a variable amount of pixels into one 16bit value.
 * To do this, the significant bits are determined and every value shifted to the right
 * After doing so it is combined into one register by shifting + oring the values
 *)
let aggregate buffer amount x y =
  let sig_bits = 16 / amount in
  let value = ref 0 in
  for i = 1 to amount do
    let new_pixel = buffer.{x + amount - i, y} lsr (8 - sig_bits) in
    value := (!value lsl sig_bits) lor new_pixel;
  done;
  !value

let get_16bit (x,y,w,h) bpp data send =
  for j = 0 to h - 1 do
    (* TODO why that? *)
    let indy = y + h - 1 - j in
    let aggregate' amount x = aggregate data amount x indy in
    let amount = 16 / match bpp with
      | `Bpp8 -> 8
      | `Bpp4 -> 4
      | `Bpp3 -> failwith "Not tightly packed"
      | `Bpp2 -> 2
      | `Bpp1 -> 1
    in
    for i = 0 to w / amount - 1 do
      let indx = x + amount*i in
      aggregate' amount indx
      |> send
    done
  done


let transmit (x,y,w,h) =
  let image_info = State.get_image_info () in
  (* Do not set the address repeatedly, only change when we use a second buffer in memory.
   * Can perhaps be used for some cool effects like moving the address 800*20 bytes along
   * to easily scroll on the screen without retransmitting the whole image. TODO
     (State.get_dev_info ()).address
     |> set_image_buffer_base_addr;
  *)
  begin
    (* Bpp1 is weird because the IT8951 does not support 1 bit transfer.
     * Therefore we disguise Bpp1 as Bpp8 with an eigth of the width
     * and use the firmware to later interpret this as a fullsize Bpp1 image
     *)
    let w = match !State.bpp with `Bpp1 -> w/8 | _ -> w in
    let args = image_info :: [x;y;w;h] in
    write_cmd_args `Load_image_area args;
  end;
  State.get_buffer ()
  |> get_16bit (x,y,w,h) !State.bpp
  |> write_data_array;
  write_cmd `Load_image_end

let display (x,y,w,h) display_mode =
  let rec wait_for_display_ready () =
    match read_reg `LUTAFSR with
    | 0 -> ()
    | _ -> wait_for_display_ready ()
  in
  wait_for_display_ready ();
  match !State.bpp with
  | `Bpp1 ->
    read_reg `UP1SR2 lor 4
    |> write_reg `UP1SR2;
    write_reg `BGVR 0x00FF; (* TODO reread sample code *)
    write_cmd_args `DPY_area [x; y; w; h; int_of_mode display_mode];
    wait_for_display_ready ();
    read_reg `UP1SR2 land (Int.neg 4)
    |> write_reg `UP1SR2
  | `Bpp2 | `Bpp4 | `Bpp8 ->
    write_cmd_args `DPY_area [x; y; w; h; int_of_mode display_mode]
  | `Bpp3 -> failwith "bpp3 not implemented"

let init () =
  (* TODO remove once code runs stable *)
  Printexc.record_backtrace true;
  (* Initialise bus *)
  (* TODO handle concurrent applications *)
  begin
    if not (Bus.init ())
    then failwith "Bus initialisation failed, insufficient permissions?"
  end;

  (* Initialise board *)
  write_reg `I80CPCR 0x0001;
  let vcom = 1500 in (* -1.53V = 1530 = 0x5FA *)
  set_vcom vcom;

  (* Initialise state used by other functions *)
  (* TODO get_dev_info modifies dev_info *)
  query_device_info ()
  |> State.set_dev_info;
  let dev_info = State.get_dev_info () in
  let buffer =
    State.create_buffer
      dev_info.width
      dev_info.height
  in
  Bigarray.Array2.fill buffer 0xFF;
  State.set_buffer buffer;

  (* Display white screen *)
  display (0, 0, dev_info.width, dev_info.height) `White

let free () =
  Bus.free ()
