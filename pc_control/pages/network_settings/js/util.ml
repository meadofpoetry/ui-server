let int_of_netmask (nm : Ipaddr.V4.t) =
  let ( >|> ) = Int32.shift_right_logical in
  let ( &&& ) = Int32.logand in
  let nm' = Ipaddr.V4.to_int32 nm in
  let rec find_greatest_one bits i =
    if bits = 0_l then i - 1 else find_greatest_one (bits >|> 1) (i + 1)
  in
  let one = nm' &&& Int32.neg nm' in
  let sz = 32 - find_greatest_one one (if one = 0_l then 33 else 0) in
  if nm <> Ipaddr.V4.Prefix.mask sz then None else Some sz

let ipv4_validation =
  Components.Textfield.(
    Custom
      {
        input_type = `Text;
        to_string = Ipaddr.V4.to_string;
        of_string =
          (fun x ->
            match Ipaddr.V4.of_string x with
            | Error _ ->
                Error "Некорректный формат IP адрес"
            | Ok _ as x -> x);
      })

let mask_validation =
  Components.Textfield.(
    Custom
      {
        input_type = `Text;
        to_string =
          (fun x ->
            Ipaddr.V4.to_string @@ Ipaddr.V4.Prefix.mask @@ Int32.to_int x);
        of_string =
          (fun x ->
            match Ipaddr.V4.of_string x with
            | Error _ ->
                Error "Некорректный формат IP адреса"
            | Ok x -> (
                match int_of_netmask x with
                | None ->
                    Error "Некорректное значение маски"
                | Some x -> Ok (Int32.of_int x) ));
      })
