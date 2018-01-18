let parse_uri =
  let open Angstrom in
  let open Common.Stream in
  let prefix   = string "udp://" in
  let dot, col = char '.', char ':' in
  let number   =
    take_while1 (function '0'..'9' -> true | _ -> false)
    >>| int_of_string
  in
  let digit    =
    number >>= fun x ->
    if x < 256
    then return x
    else fail "addr digit is gt than 255"
  in
  let port     =
    number >>= fun x ->
    if x < 65536
    then return x
    else fail "port is gt than 65535"
  in
  let addr     =
    digit
    >>= fun d1 -> dot *> digit
    >>= fun d2 -> dot *> digit
    >>= fun d3 -> dot *> digit
    >>| fun d4 -> Ipaddr.V4.make d1 d2 d3 d4
  in
  let pre      = prefix <|> (string "") in
  let parser   =
    pre *> addr <* col
    >>= fun ip   -> port
    >>| fun port -> { ip; port }
  in parse_string parser

let match_streams
      (sources : Common.Stream.source list)
      (s : Structure.structure list) : Structure.t list =
  let open Structure in
  let create structure uri (s : Common.Stream.t) =
    match s.id with
    | `Ip u when u = uri -> Some { source = (Stream s); structure }
    | _                  -> None
  in
  let from_input input uri structure =
    match input with
    | None -> { structure; source = Unknown }
    | Some input ->
       { structure
       ; source = Stream (Common.Stream.{ source = Input input
                                        ; id     = `Ip uri
                                        ; description = None }) }
  in
  let rec merge input parents acc = function
    | []    -> acc
    | x::tl ->
       match parse_uri x.uri with
       | Error _ -> merge input parents acc tl
       | Ok uri  ->
          match CCList.find_map (create x uri) parents with
          | None   -> merge input parents ((from_input input uri x)::acc) tl
          | Some s -> merge input parents (s::acc) tl
  in
  let inputs, parents = CCList.partition_map
                          Common.Stream.(function Input i -> `Left i | Parent p -> `Right p)
                          sources in
  let input = CCList.head_opt inputs in
  merge input parents [] s

let dump_streams (entries : Structure.t list) =
  let open Structure in
  List.map (fun prog -> (prog.structure.uri, Msg_conv.to_string @@ structure_to_yojson prog.structure)) entries
