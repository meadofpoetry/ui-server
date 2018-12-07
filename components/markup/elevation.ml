let base_class = "mdc-elevation"
let transition_class = base_class ^ "-transition"

let get_elevation_class (n : int) : string =
  (CSS.add_modifier base_class "z") ^ string_of_int n
