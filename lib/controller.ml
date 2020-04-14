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

(* TODO unused:
 *)

let transmit ((x,y,w,h)) =
  let get_16bit bpp data send = match bpp with
    | `Bpp8 ->
      for j = 0 to h - 1 do
        for i = 0 to w / 2 - 1 do
          let indx = x + 2*i in
          let indy = y + j in
          let value = (data.{indx + 1, indy} lsl 8) lor data.{indx, indy} in
          send value
        done
      done
    | `Bpp4 ->
      for j = 0 to h - 1 do
        for i = 0 to w / 4 - 1 do
          let indx = x + 4*i in
          let indy = y + j in
          let value =
                  ((data.{indx + 3, indy} land 0xF0) lsl 8)
              lor ((data.{indx + 2, indy} land 0xF0) lsl 4)
              lor ((data.{indx + 1, indy} land 0xF0) lsl 0)
              lor ((data.{indx + 0, indy} land 0xF0) lsr 4)
          in
          send value
        done
      done
    | `Bpp2 ->
      for j = 0 to h - 1 do
        for i = 0 to w / 8 - 1 do
          let indx = x + 8*i in
          let indy = y + j in
          let value =
                ((data.{indx + 7, indy} land 0xC0) lsl 8)
            lor ((data.{indx + 6, indy} land 0xC0) lsl 6)
            lor ((data.{indx + 5, indy} land 0xC0) lsl 4)
            lor ((data.{indx + 4, indy} land 0xC0) lsl 2)
            lor ((data.{indx + 3, indy} land 0xC0) lsl 0)
            lor ((data.{indx + 2, indy} land 0xC0) lsr 2)
            lor ((data.{indx + 1, indy} land 0xC0) lsr 4)
            lor ((data.{indx + 0, indy} land 0xC0) lsr 6)
          in
          send value
        done
      done
    | `Bpp1 ->
      for j = 0 to h - 1 do
        for i = 0 to w / 16 - 1 do
          let indx = x + 16*i in
          let indy = y + j in
          let value =
                ((data.{indx + 15, indy} land 0x80) lsl 8)
            lor ((data.{indx + 14, indy} land 0x80) lsl 7)
            lor ((data.{indx + 13, indy} land 0x80) lsl 6)
            lor ((data.{indx + 12, indy} land 0x80) lsl 5)
            lor ((data.{indx + 11, indy} land 0x80) lsl 4)
            lor ((data.{indx + 10, indy} land 0x80) lsl 3)
            lor ((data.{indx + 9, indy} land 0x80) lsl 2)
            lor ((data.{indx + 8, indy} land 0x80) lsl 1)
            lor ((data.{indx + 7, indy} land 0x80) lsl 0)
            lor ((data.{indx + 6, indy} land 0x80) lsr 1)
            lor ((data.{indx + 5, indy} land 0x80) lsr 2)
            lor ((data.{indx + 4, indy} land 0x80) lsr 3)
            lor ((data.{indx + 3, indy} land 0x80) lsr 4)
            lor ((data.{indx + 2, indy} land 0x80) lsr 5)
            lor ((data.{indx + 1, indy} land 0x80) lsr 6)
            lor ((data.{indx + 0, indy} land 0x80) lsr 7)
          in
          send value
        done
      done

    | _ -> failwith "Cant handle that!"
  in

  let start = Sys.time () in
  let image_info = State.get_image_info () in
  let w = match !State.bpp with `Bpp1 -> w/8 | _ -> w in
  let args = image_info :: [x;y;w;h] in
  (* Do not set the address repeatedly, only change when we use a second buffer in memory.
   * Can perhaps be used for some cool effects like moving the address 800*20 bytes along
   * to easily scroll on the screen without retransmitting the whole image. TODO
     (State.get_dev_info ()).address
     |> set_image_buffer_base_addr;
  *)
  write_cmd_args `Load_image_area args;
  begin
    let start = Sys.time () in
    State.get_buffer ()
    |> get_16bit !State.bpp
    |> write_data_array;
    let ende = Sys.time () in
    Printf.printf "Wrote %i bytes in %fs\n" (w*h) (ende -. start)
  end;
  write_cmd `Load_image_end;
  let ende = Sys.time () in
  Printf.printf "Transmitted image (%i x %i) in %fs\n" w h (ende -. start)

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

let init () =
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
