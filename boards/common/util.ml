module List = struct
  let direct_depth_default_ = 1000

  let find_map f l =
    let rec aux f = function
      | [] -> None
      | x :: l' -> ( match f x with Some _ as res -> res | None -> aux f l' )
    in
    aux f l

  let take n l =
    let rec direct i n l =
      match l with
      | [] -> []
      | _ when i = 0 -> safe n [] l
      | x :: l' -> if n > 0 then x :: direct (i - 1) (n - 1) l' else []
    and safe n acc l =
      match l with
      | [] -> List.rev acc
      | _ when n = 0 -> List.rev acc
      | x :: l' -> safe (n - 1) (x :: acc) l'
    in
    direct direct_depth_default_ n l

  module Assoc = struct
    let rec search_set eq acc l x ~f =
      match l with
      | [] -> f x None acc
      | (x', y') :: l' ->
          if eq x x' then f x (Some y') (List.rev_append acc l')
          else search_set eq ((x', y') :: acc) l' x ~f

    let update ~eq f x l =
      search_set eq [] l x ~f:(fun x opt_y rest ->
          match f opt_y with
          | None -> rest (* drop *)
          | Some y' -> (x, y') :: rest)
  end
end
