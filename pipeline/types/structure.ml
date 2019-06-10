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
  
  type channel =
    { number        : int
    ; service_name  : string
    ; provider_name : string
    ; pids          : (state * pid) list
    } [@@deriving yojson,eq]

  type structure =
    { id       : Application_types.Stream.t
    ; uri      : Netlib.Uri.t
    ; channels : (state * channel) list
    } [@@deriving yojson,eq]

  type t = structure list [@@deriving yojson,eq]

  let annotate ~(active:raw) ~(avail:raw) ~(stored:raw) : t =
    failwith "not impl"

  let update_stored ~(active:raw) ~(avail:raw) ~(stored:raw) =
    failwith "not impl"

  let filter ~select annotated : raw =
    failwith "not impl"

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
