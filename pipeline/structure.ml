type video_pid =
  { codec        : string
  ; resolution   : (int * int)
  ; aspect_ratio : (int * int)
  ; interlaced   : string
  ; frame_rate   : float
  } [@@deriving yojson,eq]

type audio_pid =
  { codec       : string
  ; bitrate     : string
  ; channels    : int
  ; sample_rate : int
  } [@@deriving yojson,eq]

type pid_content = Video of video_pid
                 | Audio of audio_pid
                 | Empty [@@deriving eq]
let pid_content_to_yojson = function
  | Empty   -> `String "Empty"
  | Video v -> `Assoc [("Video", (video_pid_to_yojson v))]
  | Audio a -> `Assoc [("Audio", (audio_pid_to_yojson a))]
let pid_content_of_yojson = function
  | `String "Empty" -> Ok(Empty)
  | `Assoc [("Video", v)] ->
     (match video_pid_of_yojson v with
      | Ok v -> Ok(Video v)
      | _    -> Error("failure in video_pid deserialize"))
  | `Assoc [("Audio", a)] ->
     (match audio_pid_of_yojson a with
      | Ok a -> Ok(Audio a)
      | _    -> Error("failure in audio_pid deserialize"))
  | _ -> Error("failure in pid_content deserialize")

type pid =
  { pid              : int
  ; content          : pid_content
  ; stream_type      : int
  ; stream_type_name : string
  } [@@deriving yojson,eq]

type channel =
  { number        : int
  ; service_name  : string
  ; provider_name : string
  ; pids          : pid list
  } [@@deriving yojson,eq]

type t =
  { id       : Common.Stream.ID.t
  ; uri      : Common.Url.t
  ; channels : channel list
  } [@@deriving yojson,eq]

type packed = { source    : Common.Stream.t
              ; structure : t
              } [@@deriving yojson,eq]

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

let combine ~(set : t list) (applied, strs) =
  let changed = ref false in
  let get_settings_opt stream =
    List.find_opt (fun x -> Common.Stream.ID.equal x.id stream.id)
  in
  let res =
    filter_map (fun stream ->
           let applied =
             List.find_opt (fun appl -> Common.Stream.ID.equal appl.id stream.id) applied
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

(*
module Streams = struct
  type t   = packed list [@@deriving yojson]
  let name = "streams"
  let default : t = []

  let unwrap : t -> structure list =
    List.map (fun { source; structure } -> structure )
end
 
let appeared_pids ~past ~pres =
  let flat (sl : structure list) =
    List.fold_left (fun acc s ->
        let l = List.fold_left (fun acc c ->
                    let channel = c.number in
                    let l = List.fold_left (fun acc p -> (s.id, channel, p.pid, p.to_be_analyzed)::acc)
                              [] c.pids in
                    l @ acc)
                  [] s.channels in
        l @ acc) [] sl
  in
  let rec not_in_or_diff (s,c,p,tba) = function
    | [] -> true
    | (so,co,po,tbao)::_
         when Common.Stream.ID.equal so s && co = c && po = p && not (tbao = tba) -> true
    | (so,co,po,tbao)::_
         when Common.Stream.ID.equal so s && co = c && po = p && (tbao = tba) -> false
    | _::tl -> not_in_or_diff (s,c,p,tba) tl
  in                          
  let past = flat past in
  let pres = flat pres in
  let appeared = List.fold_left (fun acc pres ->
                     let (_,_,_,tba) = pres in
                     if tba && not_in_or_diff pres past
                     then pres::acc else acc) [] pres in
  appeared
 *)

                
module Many = struct
  type nonrec t = t list
  let name = "structures"
  let default : t = []
  let to_yojson l = `List (List.map to_yojson l)
  let of_yojson = function
    | `List l -> begin try Ok (List.map (fun x -> let Ok res = of_yojson x in res) l)
                       with _ -> Error "Structure.List.of_yojson"
                 end
    | _ -> Error "Structure.List.of_yojson"
  let dump w = Yojson.Safe.to_string (to_yojson w)
  let restore s = of_yojson (Yojson.Safe.from_string s)
end
