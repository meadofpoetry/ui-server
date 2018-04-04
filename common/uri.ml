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
         } [@@deriving show, eq]

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

let to_yojson u = `String (to_string u)
let of_yojson = function
  | `String s -> of_string s
  | _         -> Error "of_yojson: bad uri"
   
let in_range (min,max) t =
  match compare min t, compare t max with
  | 1, _ | _, 1 -> false
  | _           -> true

let zero : t = { ip = Ipaddr.V4.of_int32 0l; port = 0 }

let succ : t -> t = fun { ip; port } ->
  if port = 65535
  then { ip   = Ipaddr.V4.of_int32 @@ Int32.succ @@ Ipaddr.V4.to_int32 ip
       ; port = 0
       }
  else { ip; port = succ port }

type gen = (unit -> t option)
  
let of_range : (t * t) list -> gen = fun rngs ->
  if List.is_empty rngs
  then (fun () -> None)
  else
    let rngs = ref rngs in
    (fun () ->
      match !rngs with
      | [] -> None
      | (min,max)::tl ->
         if compare min max > 0
         then failwith "of_range: bad range, min is greater than max"
         else let rval = min in
              let new_min = succ min in
              if compare min max > 0
              then rngs := tl
              else rngs := (new_min,max)::tl;
              Some rval)
    
let random : unit -> t = fun () ->
  { ip   = Ipaddr.V4.of_int32 @@ Random.int32 Int32.max_int
  ; port = Random.run ~st:(Random.get_state ()) @@ Random.int 65535
  }

exception Gen_failure
    
let gen_in_ranges : ('a * (t * t) list) list -> (('a * t) list, unit) result = fun vs ->
  let ranged, free = List.partition (fun (_,r) -> not @@ List.is_empty r) vs in
  let rec is_in a = function
    | []    -> false
    | x::tl -> if equal x a then true else is_in a tl
  in
  let rec generate used_adds = function
    | [] -> used_adds, []
    | (id, range)::tl ->
       let gen = of_range range in
       let rec loop () =
         match gen () with
         | None   -> raise_notrace Gen_failure
         | Some a ->
            if is_in a used_adds
            then loop ()
            else try
                let used, vals = generate (a::used_adds) tl in
                used, (id, a)::vals
              with Gen_failure -> loop ()
       in loop ()
  in
  let rec generate_free used_adds acc free =
    let limit = List.length used_adds in
    match free with
    | [] -> acc
    | (id,_)::tl ->
       let rec loop lim =
         let a = random () in
         if not @@ is_in a used_adds
         then generate_free used_adds ((id,a)::acc) tl
         else if lim > limit
         then raise_notrace Gen_failure
         else loop (lim + 1)
       in loop 0
  in
  try
    let used, vals = generate [] ranged in
    let frees = generate_free used [] free in
    Ok(frees @ vals)
  with Gen_failure -> Error ()
