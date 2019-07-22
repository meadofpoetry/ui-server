type t =
  { is_vertical : bool (* Is line vertical *)
  ; is_multiple : bool (* Multiple intersection detected *)
  ; is_center : bool
  ; origin : float
  }

type direction =
  | Htop
  | Hcenter
  | Hbottom
  | Vleft
  | Vcenter
  | Vright
  | Nill

let show (x : t) : string =
  Printf.sprintf "is vertical: %B, is multiple: %B, is center: %B, origin: %g"
    x.is_vertical x.is_multiple x.is_center x.origin
