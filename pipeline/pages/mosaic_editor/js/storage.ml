open Ui_templates.Storage.Local
let show_grid_lines = "show-grid-lines"
let show_snap_lines = "show-snap-lines"

let get_bool ?(default = false) key =
  match get key with
  | Some `Bool x -> x
  | _ -> default

let set_bool key (x : bool) =
  put key (`Bool x)
