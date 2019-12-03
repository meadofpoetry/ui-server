type t =
  { is_vertical : bool (* Is line vertical *)
  ; is_multiple : bool (* Multiple intersection detected *)
  ; is_center : bool
  ; origin : float }

type direction =
  | Htop
  | Hcenter
  | Hbottom
  | Vleft
  | Vcenter
  | Vright
  | Nill
