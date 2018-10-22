(* NOTE: just an assumption *)
(*
type widget_type = Video
                 | Audio
                 | Subtitles
                 | Teletext
                 | Qos          (* qos errors/events *)
                 | Qoe          (* qoe errors/events *)
                 | Clock        (* digital or analog clock *)
                 | Text         (* static text *)
                 | Image        (* image (from file or from url, maybe) *)
                 | Source       (* text with channel(stream,input,etc) name*)
                 | Eit          (* text with EIT info *)
                 | Service_info (* text with service desrciption (resolution,codec,etc) *)
                 | Icons_bar    (* status bar with availability indication of eit, scte35, teletext etc  *)
[@@deriving yojson]
 *)

let name = "wm"

type widget_type = Video | Audio [@@deriving eq]

let widget_type_equal typ_1 typ_2 =
  match typ_1, typ_2 with
  | Video, Video | Audio, Audio -> true
  | _ -> false

let widget_type_of_yojson = function
  | `String "Video" -> Ok(Video)
  | `String "Audio" -> Ok(Audio)
  | _ -> Error "widget_type_of_yojson"
let widget_type_to_yojson = function
  | Video -> `String "Video"
  | Audio -> `String "Audio"

type domain = Nihil : domain
            | Chan  : { stream  : Common.Stream.ID.t
                      ; channel : int } -> domain
            [@@deriving eq]
let domain_of_yojson = function
  | `String "Nihil" -> Ok(Nihil)
  | `Assoc ["Chan",
            `Assoc ["stream", `String id;
                    "channel", `Int channel]] ->
     Ok(Chan { stream = Common.Stream.ID.of_string id; channel })
  | `Assoc ["Chan",
            `Assoc ["stream", `String id;
                    "channel", `Intlit channel]] ->
     Ok(Chan { stream = Common.Stream.ID.of_string id; channel = int_of_string channel })
  | _ -> Error "domain_of_yojson: bad json"
let domain_to_yojson = function
  | Nihil -> `String "Nihil"
  | Chan { stream; channel } ->
     `Assoc ["Chan", `Assoc ["stream", `String (Common.Stream.ID.to_string stream);
                             "channel", `Int channel]]
         
type background = (* NOTE incomplete *)
  { color : int } [@@deriving yojson]

type position =
  { left   : int
  ; top    : int
  ; right  : int
  ; bottom : int
  } [@@deriving yojson,eq]

type widget =
  { type_       : widget_type [@key "type"]
  ; domain      : domain
  ; pid         : int option
  ; position    : position
  ; layer       : int
  ; aspect      : ((int * int) option [@default None])
  ; description : string
  } [@@deriving yojson,eq]

type container =
  { position : position
  ; widgets  : (string * widget) list
  } [@@deriving yojson,eq]

type t =
  { resolution : int * int
  ; widgets    : (string * widget) list
  ; layout     : (string * container) list
  } [@@deriving yojson]

let update _ b = b

let default = { resolution = 1280, 720
              ; widgets   = []
              ; layout    = []
              }

let aspect_to_string = function
  | None -> "none"
  | Some (x,y) -> Printf.sprintf "%dx%d" x y
            
let dump w = Yojson.Safe.to_string (to_yojson w)

let restore s = of_yojson (Yojson.Safe.from_string s)

let combine ~set wm =
  let changed = ref false in
  let rec filter_container = function
    | []        -> []
    | (n,w)::tl ->
       match List.find_opt (fun (name,_) -> name = n) wm.widgets with
       | None          -> filter_container tl
       | Some (_,widg) ->
          if not (widg.aspect = w.aspect)
          then filter_container tl
          else begin
              if not (widg.position = w.position && widg.layer = w.layer) then changed := true;
              (n, { widg with position = w.position; layer = w.layer }) :: (filter_container tl)
            end
  in
  let rec filter_layout : (string * container) list -> (string * container) list = function
    | []        -> []
    | (n,c)::tl ->
       (n, { c with widgets = filter_container c.widgets } ) :: (filter_layout tl)
  in
  let layout = filter_layout set.layout in
  if !changed
  then `Changed { wm with resolution = set.resolution; layout }
  else `Kept wm
