open Application_types

include Qoe_backend_types__Wm.Make (Stream.ID)
   
let name = "wm"

type annotated =
  { resolution : int * int
  ; widgets    : (string * state * widget) list
  ; layout     : (string * state * container) list
  } [@@deriving yojson, eq]
and state = [`Absent | `Presented]
  
let update _ (b : t) = b

let default : t = { resolution = 1280, 720
                  ; widgets   = []
                  ; layout    = []
                  }

let aspect_to_string = function
  | None -> "none"
  | Some (x,y) -> Printf.sprintf "%dx%d" x y
            
let to_string w = Yojson.Safe.to_string (to_yojson w)

let of_string s =
  match of_yojson (Yojson.Safe.from_string s) with
  | Ok v -> v
  | Error e -> failwith e

let combine ~(set : t) (wm : t) =
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
