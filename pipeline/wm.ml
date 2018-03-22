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

type background = (* NOTE incomplete *)
  { color : int } [@@deriving yojson]

type position =
  { left   : int
  ; top    : int
  ; right  : int
  ; bottom : int
  } [@@deriving yojson,eq]

type widget =
  { type_       : string [@key "type"]
  ; domain      : string
  ; position    : position
  ; layer       : int
  ; aspect      : (int * int)
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
