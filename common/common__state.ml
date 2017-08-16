type t =
  { streams  : Common__streams.t  option [@default None]
  ; settings : Common__settings.t option [@default None]
  ; graph    : Common__graph.t option [@default None]
  ; wm       : Common__wm.t option [@default None]
  } [@@deriving yojson, lens]

open Common__opt_update
  
let update a b =
  { streams  = opt_update Common__streams.update a.streams b.streams
  ; settings = opt_update Common__settings.update a.settings b.settings
  ; graph    = opt_update Common__graph.update a.graph b.graph
  ; wm       = opt_update Common__wm.update a.wm b.wm
  }

let default : t =
  { streams  = (Some Common__streams.default )
  ; settings = (Some Common__settings.default )
  ; graph    = (Some Common__graph.default )
  ; wm       = (Some Common__wm.default )
  }

    (*
      type xy = { x : string; y : int } [@@deriving lens];;
      type st = { out : string; i : t option } [@@deriving lens];;
      let mod_opt f v o = Option.(o >>= fun s -> return ((f ^= v) s));;
      (st_i ^%= (mod_opt xy_y 17)) @@ { out = "12"; i = Some {x = "3"; y = 5} };;
     *)
