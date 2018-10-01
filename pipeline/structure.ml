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
  ; to_be_analyzed   : bool
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

type structure =
  { id       : int32 
  ; uri      : Common.Url.t
  ; channels : channel list
  } [@@deriving yojson,eq]

type packed = { source    : Common.Stream.t
              ; structure : structure
              } [@@deriving yojson,eq]
type t = packed [@@deriving eq]

let combine_pid ~changed ~set x =
  if not (x.to_be_analyzed = set.to_be_analyzed) then changed := true;
  { x with to_be_analyzed = set.to_be_analyzed }

let combine_channel ~changed ~set x =
  let rec combine_pids set_pids = function
    | []    -> []
    | x::tl ->
       match List.find_opt (fun p -> x.pid = p.pid) set_pids with
       | None   -> x :: (combine_pids set_pids tl)
       | Some p -> (combine_pid ~changed ~set:p x) :: (combine_pids set_pids tl)
  in { x with pids = combine_pids set.pids x.pids }

let combine_structure ~changed ~set x =
  let rec combine_channels set_chans = function
    | []    -> []
    | x::tl ->
       match List.find_opt (fun c -> x.number = c.number) set_chans with
       | None   -> x :: (combine_channels set_chans tl)
       | Some c -> (combine_channel ~changed ~set:c x) :: (combine_channels set_chans tl)
  in { x with channels = combine_channels set.channels x.channels }
       
module Structures = struct
  type t   = structure list [@@deriving yojson]
  let name = "structures"
  let default : t = []
  let dump w = Yojson.Safe.to_string (to_yojson w)
  let restore s = of_yojson (Yojson.Safe.from_string s)
  let combine ~set strs =
    let changed = ref false in
    let res = List.map (fun s -> match List.find_opt (fun x -> x.uri = s.uri) set with
                       | None   -> s
                       | Some x -> combine_structure ~changed ~set:x s)
                strs
    in if !changed then `Changed res else `Kept strs
end
       
module Streams = struct
  type t   = packed list [@@deriving yojson]
  let name = "streams"
  let default : t = []

  let unwrap : t -> structure list =
    List.map (fun { source; structure } -> structure )
end
