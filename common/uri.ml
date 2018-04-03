open Containers

type ip   = Ipaddr.V4.t
let pp_ip = Ipaddr.V4.pp_hum
let ip_to_yojson ip =
  Ipaddr.V4.to_string ip
  |> fun s -> `String s
let ip_of_yojson = function
  | `String s -> Ipaddr.V4.of_string s
                 |> (function Some ip -> Ok ip | None -> Error ("ip_of_yojson: bad ip: " ^ s))
  | _ -> Error "ip_of_yojson: bad js"
let equal_ip a1 a2 = Int32.equal (Ipaddr.V4.to_int32 a1) (Ipaddr.V4.to_int32 a2)

type t = { ip   : ip
         ; port : int
         } [@@deriving yojson, show, eq]

let compare (t1:t) (t2:t) =
  let c = Ipaddr.V4.compare t1.ip t2.ip in
  if c <> 0 then c else compare t1.port t2.port

let to_string t = Printf.sprintf "udp://%s:%d" (Ipaddr.V4.to_string t.ip) t.port
let of_string s =
  let open Angstrom in
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
  in parse_string parser s

let in_range (min,max) t =
  match compare min t, compare t max with
  | 1, _ | _, 1 -> false
  | _           -> true
