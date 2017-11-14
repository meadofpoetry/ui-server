type t =
  { streams  : Streams.entries option [@default None]
  ; settings : Settings.t option [@default None]
  ; graph    : Graph.t option [@default None]
  ; wm       : Wm.t option [@default None]
  } [@@deriving yojson]

open Opt_update
  
let update a b =
  { streams  = opt_update Streams.update a.streams b.streams
  ; settings = opt_update Settings.update a.settings b.settings
  ; graph    = opt_update Graph.update a.graph b.graph
  ; wm       = opt_update Wm.update a.wm b.wm
  }

let default : t =
  { streams  = (Some Streams.default )
  ; settings = (Some Settings.default )
  ; graph    = (Some Graph.default )
  ; wm       = (Some Wm.default )
  }

    (*
      type xy = { x : string; y : int } [@@deriving lens];;
      type st = { out : string; i : t option } [@@deriving lens];;
      let mod_opt f v o = Option.(o >>= fun s -> return ((f ^= v) s));;
      (st_i ^%= (mod_opt xy_y 17)) @@ { out = "12"; i = Some {x = "3"; y = 5} };;
     *)
