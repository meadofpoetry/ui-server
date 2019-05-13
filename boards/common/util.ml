module Pair = struct

  type ('a, 'b) t = 'a * 'b

  let equal (f : 'a -> 'a -> bool) (g : 'b -> 'b -> bool)
        (a1, b1) (a2, b2) : bool =
    f a1 a2 && g b1 b2

end

module Option = struct

  type 'a t = 'a option

  let equal (f : 'a -> 'a -> bool) a b = match a, b with
    | None, None -> true
    | Some _, None | None, Some _ -> false
    | Some a, Some b -> f a b

end

module List = struct
  include List

  type 'a t = 'a list

  let rec equal (f : 'a -> 'a -> bool) l1 l2 = match l1, l2 with
    | [], [] -> true
    | [], _ | _, [] -> false
    | x1 :: l1', x2 :: l2' -> f x1 x2 && equal f l1' l2'

  let mem ~eq x l =
    let rec search eq x l = match l with
      | [] -> false
      | y::l' -> eq x y || search eq x l'
    in search eq x l

  let find_map f l =
    let rec aux f = function
      | [] -> None
      | x :: l' ->
         match f x with
         | Some _ as res -> res
         | None -> aux f l'
    in aux f l

  let partition_map f l =
    let rec iter f l1 l2 l = match l with
      | [] -> List.rev l1, List.rev l2
      | x :: tl ->
         match f x with
         | `Left y -> iter f (y :: l1) l2 tl
         | `Right y -> iter f l1 (y :: l2) tl
         | `Drop -> iter f l1 l2 tl in
    iter f [] [] l

  let filter_map f l =
    let rec recurse acc l = match l with
      | [] -> List.rev acc
      | x :: l' ->
         let acc' = match f x with
           | None -> acc
           | Some y -> y :: acc in
         recurse acc' l'
    in recurse [] l

  module Assoc = struct

    let rec search_set eq acc l x ~f = match l with
      | [] -> f x None acc
      | (x', y') :: l' ->
         if eq x x'
         then f x (Some y') (List.rev_append acc l')
         else search_set eq ((x', y') :: acc) l' x ~f

    let set ~eq x y l =
      search_set eq [] l x
        ~f:(fun x _ l -> (x,y)::l)

    let update ~eq f x l =
      search_set eq [] l x
        ~f:(fun x opt_y rest ->
          match f opt_y with
          | None -> rest (* drop *)
          | Some y' -> (x, y') :: rest)
  end
end
