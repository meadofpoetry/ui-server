module List = struct
  type 'a t = 'a list

  let rec equal (f : 'a -> 'a -> bool) (l1 : 'a t) l2 =
    match (l1, l2) with
    | [], [] -> true
    | [], _ | _, [] -> false
    | x1 :: l1', x2 :: l2' -> f x1 x2 && equal f l1' l2'
end

module Option = struct
  type 'a t = 'a option

  let equal (f : 'a -> 'a -> bool) (a : 'a t) b =
    match (a, b) with
    | None, None -> true
    | Some _, None | None, Some _ -> false
    | Some a, Some b -> f a b
end

module Result = struct
  type ('a, 'e) t = ('a, 'e) result

  let equal ~ok ~error a b =
    match (a, b) with
    | Ok a, Ok b -> ok a b
    | Error a, Error b -> error a b
    | _ -> false
end

module Pair = struct
  type ('a, 'b) t = 'a * 'b

  let equal f1 f2 (a : ('a, 'b) t as 'p) (b : 'p) =
    f1 (fst a) (fst b) && f2 (snd a) (snd b)
end

module Int = struct
  type t = int

  let equal a b = a = b
end
