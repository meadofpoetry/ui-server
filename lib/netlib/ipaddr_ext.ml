module V4 = struct
  include Ipaddr.V4

  let equal (t1:t) (t2:t) =
    Int32.equal (to_int32 t1) (to_int32 t2)

  let to_yojson (x:t) : Yojson.Safe.json =
    let s = to_string x in `String s

  let of_yojson : Yojson.Safe.json -> (t,string) result = function
    | `String s -> (match of_string s with
                    | Ok _ as a -> a
                    | Error (`Msg m) -> Error ("bad address: " ^ m))
    | _ -> Error "not an ipv4 addr"

  let zero = Ipaddr.V4.of_int32 0l

  let succ ip =
    Ipaddr.V4.of_int32 @@ Int32.succ @@ Ipaddr.V4.to_int32 ip

  let in_range (min, max) ip =
    match compare min ip, compare ip max with
    | 1, _ | _, 1 -> false
    | _           -> true

  let rec range_to_seq (r : (t * t) list) : t Seq.t =
    fun () ->
    match r with
    | [] -> Seq.Nil
    | (min,max)::tl ->
       match compare min max < 0 with
       | false -> Seq.Cons (min, range_to_seq tl)
       | true  -> Seq.Cons (min, range_to_seq ((succ min, max)::tl))

  let gen_in_ranges ?(forbidden : t list = []) (vs : ('a * (t * t) list) list)
      : ('a * t) list =
    let ranged, free = List.partition (fun (_,r) -> not @@ (r = [])) vs in
    let rec is_in a = function
      | []    -> false
      | x::tl -> if equal x a then true else is_in a tl
    in
    let rec generate used_adds acc = function
      | [] -> used_adds, List.concat @@ List.map List.of_seq acc
      | (id, range)::tl ->
         let range = range_to_seq range in
         let used = ref used_adds in
         let vals = Seq.filter_map (fun ad ->
                        if is_in ad !used
                        then None
                        else (used := ad::(!used); Some (id, ad)))
                      range
         in generate !used (vals::acc) tl
    in
    let rec generate_free used_adds acc free =
      let limit = List.length used_adds in
      match free with
      | [] -> acc
      | (id,_)::tl ->
         let rec loop lim =
           let ad = of_int32 @@ Random.int32 Int32.min_int in
           if not @@ is_in ad used_adds
           then generate_free used_adds ((id,ad)::acc) tl
           else if lim > limit
           then raise Exit
           else loop (lim + 1)
         in loop 0
    in
    try
      let used, vals = generate forbidden [] ranged in
      let frees = generate_free used [] free in
      frees @ vals
    with _ -> []
    
end

module V6 = struct
  include Ipaddr.V6

  let equal (t1:t) (t2:t) =
    let eq (xl,yl) (xr,yr) = Int64.equal xl xr && Int64.equal yl yr in
    eq (to_int64 t1) (to_int64 t2)

  let to_yojson (x:t) : Yojson.Safe.json =
    let s = to_string x in `String s

  let of_yojson : Yojson.Safe.json -> (t,string) result = function
    | `String s -> (match of_string s with
                    | Ok _ as a -> a
                    | Error (`Msg m) -> Error ("bad address: " ^ m))
    | _ -> Error "not an ipv6 addr"

end

include (Ipaddr:module type of struct include Ipaddr end
                               with module V4 := Ipaddr.V4
                               with module V6 := Ipaddr.V6)
