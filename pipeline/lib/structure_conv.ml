open Containers

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
      (sources : (string * Common.Stream.t) list ref)
      (sl : Structure.structure list) : Structure.t list =
  let open Structure in
  let rec merge (sources : (string * Common.Stream.t) list) structure =
    match sources with
    | [] -> { source = Unknown; structure }
    | (uri, s)::ss ->
       if String.equal uri structure.uri
       then { source = Stream s; structure }
       else merge ss structure
  in
  List.map (merge !sources) sl

let dump_streams (entries : Structure.t list) =
  let open Structure in
  List.map (fun prog -> (prog.structure.uri, Msg_conv.to_string @@ structure_to_yojson prog.structure)) entries
