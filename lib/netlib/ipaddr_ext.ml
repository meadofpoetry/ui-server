module V4 = struct
  include Ipaddr.V4

  type range = t * t

  let multicast =
    (Ipaddr.V4.of_string_exn "224.0.0.0",
     Ipaddr.V4.of_string_exn "239.255.255.255")

  let range_of_pair (min, max) =
    if min <= max
    then Some (min, max)
    else None

  let range_to_pair (min, max) = (min, max)

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

  let pred ip =
    Ipaddr.V4.of_int32 @@ Int32.pred @@ Ipaddr.V4.to_int32 ip

  let max_ip =
    Ipaddr.V4.of_string_exn "255.255.255.255"

  let min_ip =
     Ipaddr.V4.of_string_exn "0.0.0.0"

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

  let is_none = function None -> true | _ -> false

  (* let get_exn = function Some x -> x | None -> failwith "get_exn" *)

  (* TODO and range type with invarian min < max *)
  (* Brutally solved by a random assingment
     TODO consider SAT-like approach *)
  let gen_in_ranges ?(forbidden : t list = []) ~allowed (vs : ('a * (t * t) list) list)
      : ('a * t) list =

    (* Debug

    let forb =
      Printf.sprintf "Forbidden: [%s]"
      @@ String.concat "; " (List.map to_string forbidden)
    in
    let all =
      Printf.sprintf "Allowed: [%s]"
        ("(" ^ (to_string (fst allowed)) ^ " .. " ^ (to_string (snd allowed)) ^ ")")
    in
    let dat =
      Printf.sprintf "Input: [%s]"
      @@ String.concat "; "
      @@ List.map (fun (_id, rngs) ->
             let rngs = List.map (fun rng ->
                            let min, max = range_to_pair rng in
                            "(" ^ (to_string min) ^ " .. " ^ (to_string max) ^ ")")
                          rngs
             in "< 'a: " ^ String.concat "; " rngs ^ ">")
           vs
    in
    
    print_endline forb;
    print_endline all;
    print_endline dat;

    End Debug *)
    
    let random_in ~ranges =
      let len = List.length ranges in
      assert (len > 0);
      let random_range = Random.int len in
      let min, max = List.nth ranges random_range in
      if max > min
      then let min, max = to_int32 min, to_int32 max in
           Int32.add (Random.int32 Int32.(sub max min)) min
           |> of_int32
      else min
    in
    let split_ranges ~excluded ranges =
      let exclude (min, max) =
        if in_range (min, max) excluded
        then [ min, pred excluded
             ; succ excluded, max
             ] (* TODO check if overlap *)
        else [ min, max ]
      in List.concat @@ List.map exclude ranges
    in
    let constraint_by_allowed ~allowed_ranges ranges =
      let intersect (allowed_min, allowed_max) (min, max) =
        let min =
          if min < allowed_min then allowed_min
          else if min > allowed_max then allowed_max
          else min
        in
        let max =
          if max < allowed_min then allowed_min
          else if max > allowed_max then allowed_max
          else max
        in min, max
      in
      let rec constrain ~allowed = function
        | [] ->
           [ allowed ]
        | [h] ->
           [ intersect allowed h ]
        | h::tl ->
           (intersect allowed h)::(constrain ~allowed tl)
      in
      List.fold_left (fun ranges allowed ->
          constrain ~allowed ranges)
        ranges
        allowed_ranges
    in
    let rec solve = function
      | [] -> Some []
      | (id, ranges)::rest ->
         let tries = ref 10 in
         let result = ref None in
         while is_none !result && !tries > 0 do
           let ip = random_in ~ranges in
           let rest' = List.map (fun (id,ranges) ->
                           id, split_ranges ~excluded:ip ranges)
                         rest
           in
           match solve rest' with
           | Some result' ->
              result := Some ((id, ip)::result')
           | None ->
              decr tries
         done;
         !result
    in

    let allowed = List.fold_left (fun ranges excluded ->
                      split_ranges ~excluded ranges)
                    [ allowed ]
                    forbidden
    in
    let constrained, unconstrained = List.partition (fun (_,r) -> not @@ (r = [])) vs in
    
    let constrained = List.map
                        (fun (id, ranges) ->
                          id, constraint_by_allowed ~allowed_ranges:allowed ranges)
                        constrained
    in
    let unconstrained = List.map
                          (fun (id,_) -> (id, allowed))
                          unconstrained
    in
    
    match solve (constrained @ unconstrained) with
    | None ->
       []
    | Some used ->
       used

    (* Debug
    
    let dat =
      Printf.sprintf "Output: [%s]"
      @@ String.concat "; "
      @@ List.map (fun (_id, ip) ->
             "< 'a: " ^ to_string ip ^ ">")
           res
    in

    print_endline dat;

    res

      End debug *)
    
    
  (*              
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
              *)
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
