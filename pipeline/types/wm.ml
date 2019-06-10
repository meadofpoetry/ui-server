open Application_types

include Qoe_backend_types.Wm.Make (Stream.ID)
  
let default : t = { resolution = 1280, 720
                  ; widgets   = []
                  ; layout    = []
                  }

module Annotated = struct

  type raw = t

  type state = [`Active | `Stored ] [@@deriving yojson, eq]

  type container =
    { position : position
    ; widgets  : (string * state * widget) list
    } [@@deriving yojson, eq]

  type t =
    { resolution : int * int
    ; widgets    : (string * widget) list
    ; layout     : (string * state * container) list
    } [@@deriving yojson, eq]

  let annotate ~active ~stored =
    failwith "not impl"

  let update_stored ~active ~stored =
    failwith "not impl"

  let filter ~select annotated =
    failwith "not impl"

end

(*
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
 *)
