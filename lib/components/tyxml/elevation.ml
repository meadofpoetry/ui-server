module CSS = struct
  let root = "mdc-elevation"

  (** Applies the correct css rules to transition an element between elevations. *)
  let transition = root ^ "-transition"

  (** Sets the elevation to the (N)dp, where 1 <= N <= 24 *)
  let elevation (n : int) : string = BEM.add_modifier root "z" ^ string_of_int n
end
