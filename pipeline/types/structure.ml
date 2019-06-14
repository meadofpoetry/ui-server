open Application_types

include Qoe_backend_types.Structure.Make (Stream.ID) (Netlib.Uri)

module Packed = struct
      
  type nonrec t = { source    : Stream.t
                  ; structure : t
                  }

  let to_yojson { source; structure } =
    `Assoc [ "source", Stream.to_yojson source
           ; "structure", to_yojson structure ]

  let ( >>= ) m f = match m with Ok v -> f v | Error _ as e -> e
    
  let of_yojson = function
    | `Assoc [ "source", src; "structure", str ] ->
       Stream.of_yojson src
       >>= fun source ->
       of_yojson str
       >>= fun structure ->
       Ok { source; structure }
    | _ -> Error "Packed.of_yojson"

end

let pids =
  List.fold_left (fun acc (s : t) ->
      let channels = List.fold_left (fun acc c ->
                         let pids = List.fold_left (fun acc p ->
                                        (s.id, c.number, p.pid)::acc)
                                      [] c.pids
                         in pids @ acc)
                       [] s.channels
      in channels @ acc)
    []
(*
let unwrap f = function
  | None -> []
  | Some x -> f x

let filter_map f lst =
  let rec fmap acc = function
    | [] -> List.rev acc
    | h::tl -> match f h with
               | None -> fmap acc tl
               | Some x -> fmap (x::acc) tl
  in fmap [] lst

let cons_opt h tl = match h with
  | Some h -> h::tl
  | None -> tl

let combine_pid ~changed ~applied ~set =
  match applied with
  | Some s -> s
  | None ->
     changed := true;
     set
     
let combine_channel ~changed ~applied ~set x =
  let rec combine_pids set_pids = function
    | []    -> []
    | h::tl ->
       let applied = List.find_opt (fun p -> h.pid = p.pid) applied in
       match List.find_opt (fun p -> h.pid = p.pid) set_pids with
       | None   -> cons_opt applied (combine_pids set_pids tl)
       | Some set ->
          (combine_pid ~changed ~applied ~set)
          :: (combine_pids set_pids tl)
  in match combine_pids set.pids x.pids with
     | [] -> None
     | p  -> Some { x with pids = p }
   
let combine_structure ~changed ~set ~applied x =
  let get_settings_opt chan = List.find_opt (fun c -> chan.number = c.number) in
  let rec combine_channels set_chans = function
    | []    -> []
    | h::tl ->
       let applied =
         List.find_opt (fun x -> x.number = h.number) applied
       in
       match get_settings_opt h set_chans with
       | None -> cons_opt applied (combine_channels set_chans tl)
       | Some set ->
          let applied = unwrap (fun x -> x.pids) applied in
          cons_opt
            (combine_channel ~changed ~applied ~set h)
            (combine_channels set_chans tl)
  in match combine_channels set.channels x.channels with
     | [] -> None
     | c  -> Some { x with channels = c }

let combine ~(set : t list) applied strs =
  let changed = ref false in
  let get_settings_opt stream =
    List.find_opt (fun x -> Stream.ID.equal x.id stream.id)
  in
  let res =
    filter_map (fun stream ->
           let applied =
             List.find_opt (fun appl -> Stream.ID.equal appl.id stream.id) applied
           in
           match get_settings_opt stream set with
           | None -> applied
           | Some set ->
              let applied = unwrap (fun x -> x.channels) applied in
              combine_structure ~changed ~set ~applied stream)
      strs
  in
  if !changed
  then `Changed res
  else `Kept applied
       *)
module Annotated = struct
  
  type state = [`Active_and_stored | `Avail | `Stored ] [@@deriving yojson,eq]

  type raw = t list [@@deriving yojson,eq]

  type raw_pid = pid [@@deriving eq]

  type raw_channel = channel

  type raw_structure = t
  
  type channel =
    { number        : int
    ; service_name  : string
    ; provider_name : string
    ; pids          : (state * pid) list
    } [@@deriving yojson,eq]

  type structure =
    { id       : Application_types.Stream.ID.t
    ; uri      : Netlib.Uri.t
    ; channels : (state * channel) list
    } [@@deriving yojson,eq]

  type t = structure list [@@deriving yojson,eq]

  (* TODO remove after 4.08 *)
  let rec filter_opt = function
    | [] -> []
    | None::tl -> filter_opt tl
    | (Some h)::tl -> h::(filter_opt tl)

  let opt_map_any ~f opt1 opt2 opt3 =
    match opt1 with
    | Some v -> Some (f `First v)
    | None ->
       match opt2 with
       | Some v -> Some (f `Second v)
       | None ->
          match opt3 with
          | Some v -> Some (f `Third v)
          | None -> None
         
  let opt_get_list opt ~deref =
    match opt with
    | None -> []
    | Some l -> deref l

  (* find matches for main in l1 and l2 and 
     removes them from l1 and l2
     Rearranges resulting tuples via rearrange *)
  let match_and_filter ~pred ~rearrange ~main l1 l2 =
    let rec find_and_filter pred = function
      | [] -> None, []
      | h::tl ->
         if pred h
         then Some h, tl
         else
           let res, tl' = find_and_filter pred tl in
           res, h::tl'
    in
    let rec traverse combine acc filt_l1 filt_l2 = function
      | [] -> acc, filt_l1, filt_l2
      | h::tl ->
         let el_1, filt_l1' = find_and_filter (pred h) filt_l1 in
         let el_2, filt_l2' = find_and_filter (pred h) filt_l2 in
         traverse
           combine
           ((combine (Some h) el_1 el_2)::acc)
           filt_l1'
           filt_l2'
           tl
    in
    traverse rearrange [] l1 l2 main
    
  let annotate ~(active:raw) ~(avail:raw) ~(stored:raw) : t =

    let merge_pids ?active ?avail ?stored () =
      opt_map_any active avail stored
        ~f:(fun prior (pid : raw_pid) ->
          match prior with
          | `Third -> `Stored, pid
          | `Second -> `Avail, pid
          | `First -> `Active_and_stored, pid)
    in
    
    let merge_channels ?active ?avail ?stored () =
      let eq_pid_num (p1 : raw_pid) (p2 : raw_pid) =
        equal_raw_pid p1 p2 (* TODO check this *)
        (*p1.pid = p2.pid*)
      in
      let deref (c : raw_channel) = c.pids in
      let active_args, avail', stored' =
        match_and_filter
          ~pred:eq_pid_num
          ~rearrange:(fun a b c -> (a, b, c))
          ~main:(opt_get_list active ~deref)
          (opt_get_list avail ~deref)
          (opt_get_list stored ~deref)
      in
      let avail_args, stored'', _ =
        match_and_filter
          ~pred:eq_pid_num
          ~rearrange:(fun b c a -> (a,b,c))
          ~main:avail'
          stored'
          []
      in
      let stored_args = List.map (fun x -> (None,None,Some x)) stored'' in
      let pids =
        (stored_args @ avail_args @ active_args)
        |> List.filter (function (None,None,None) -> false | _ -> true)
        |> List.map (fun (active,avail,stored) ->
               merge_pids ?active ?avail ?stored ())
        |> filter_opt
      in
      opt_map_any active avail stored
        ~f:(fun prior (ch : raw_channel) ->
          let c = { number        = ch.number
                  ; service_name  = ch.service_name
                  ; provider_name = ch.provider_name
                  ; pids
                  }
          in
          match prior with
          | `Third -> `Stored, c
          | `Second -> `Avail, c
          | `First -> `Active_and_stored, c)
    in
    
    let merge_structures ?active ?avail ?stored () : structure option =
      let eq_chan_num (ch1 : raw_channel) (ch2 : raw_channel) =
        ch1.number = ch2.number
      in
      let deref (s : raw_structure) = s.channels in
      let active_args, avail', stored' =
        match_and_filter
          ~pred:eq_chan_num
          ~rearrange:(fun a b c -> (a,b,c))
          ~main:(opt_get_list active ~deref)
          (opt_get_list avail ~deref)
          (opt_get_list stored ~deref)
      in
      let avail_args, stored'', _ =
        match_and_filter
          ~pred:eq_chan_num
          ~rearrange:(fun b c a -> (a,b,c))
          ~main:avail'
          stored'
          []
      in
      let stored_args = List.map (fun x -> (None, None, Some x)) stored'' in
      let channels =
        (stored_args @ avail_args @ active_args)
        |> List.filter (function (None,None,None) -> false | _ -> true)
        |> List.map
             (fun (active, avail, stored) ->
               merge_channels ?active ?avail ?stored ())
        |> filter_opt
      in
      opt_map_any active avail stored
        ~f:(fun _ (s : raw_structure) -> ({ id = s.id; uri = s.uri; channels } : structure))
    in

    let eq_struct_id (s1 : raw_structure) (s2 : raw_structure) =
      s1.id = s2.id
    in
    let active_structs, avail', stored' =
      match_and_filter
        ~pred:eq_struct_id
        ~rearrange:(fun a b c -> (a,b,c))
        ~main:active
        avail
        stored
    in
    let avail_structs, stored'', _ =
      match_and_filter
        ~pred:eq_struct_id
        ~rearrange:(fun b c a -> (a,b,c))
        ~main:avail'
        stored'
        []
    in
    let stored_structs = List.map (fun x -> (None,None,Some x)) stored'' in
    (stored_structs @ avail_structs @ active_structs)
    |> List.map (fun (active, avail, stored) ->
           merge_structures ?active ?avail ?stored ())
    |> filter_opt

  let update_stored ~(active:raw) ~(avail:raw) ~(stored:raw) =
    let updated = ref false in

    let for_each_pid ?active_chan ?stored_chan pid =
      opt_get_list active_chan ~deref:(fun (c : raw_channel) -> c.pids)
      |> List.find_opt (equal_raw_pid pid)
      |> function
        | Some _ ->
           (* Pid is already active *)
           pid
        | None ->
           (* Pid is not yet active *)
           opt_get_list stored_chan ~deref:(fun (c : raw_channel) -> c.pids)
           |> List.find_opt (equal_raw_pid pid)
           |> function
             | None ->
                (* No settings for that pid *)
                pid
             | Some stored ->
                (* Settings were found *)
                updated := true;
                stored
    in

    let for_each_channel ?active_struct ?stored_struct ch =
      let eq_chan (c1 : raw_channel) (c2 : raw_channel) =
        c1.number = c2.number
      in
      let active_chan =
        opt_get_list active_struct ~deref:(fun (c : raw_structure) -> c.channels)
        |> List.find_opt (eq_chan ch)
      in
      let stored_chan =
        opt_get_list stored_struct ~deref:(fun (c : raw_structure) -> c.channels)
        |> List.find_opt (eq_chan ch)
      in
      let pids = List.map (for_each_pid ?active_chan ?stored_chan) ch.pids in
      { ch with pids }
    in
    
    let for_each_structure s =
      let eq_struct (s1 : raw_structure) (s2 : raw_structure) =
        s1.id = s2.id
      in
      let active_struct = List.find_opt (eq_struct s) active in
      let stored_struct = List.find_opt (eq_struct s) stored in
      let channels = List.map (for_each_channel ?active_struct ?stored_struct) s.channels in
      { s with channels }
    in

    let res = List.map for_each_structure avail in
    match !updated with
    | true -> `Changed res
    | false -> `Kept active

(*
  let filter ~select annotated : raw =
    failwith "not impl"
 *)
end
                
module Many = struct
  type nonrec t = t list
  let name = "structures"
  let default : t = []
  (* TODO test this *)
  let equal l r =
    try
      let in_r, not_in_r = List.partition
                             (fun el -> List.exists (fun er -> equal el er) r) l in
      if not_in_r <> [] then raise_notrace Not_found;
      List.iter (fun er -> if not @@ List.exists (fun el -> equal el er) in_r
                           then raise_notrace Not_found) r;
      true
    with Not_found -> false
    
  let to_yojson l = `List (List.map to_yojson l)
  let of_yojson = function
    | `List l -> begin try Ok (List.map (fun x -> let [@warning "-8"] Ok res = of_yojson x in res) l)
                       with _ -> Error "Structure.List.of_yojson"
                 end
    | _ -> Error "Structure.List.of_yojson"
  let to_string w = Yojson.Safe.to_string (to_yojson w)
  let of_string s =
    match of_yojson (Yojson.Safe.from_string s) with
    | Ok v -> v
    | Error e -> failwith e
end
