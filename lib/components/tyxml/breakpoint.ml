type 'a v = int * 'a

type 'a t = 'a * 'a v list

let make ?(points = []) (upper : 'a) : 'a t = upper, points

let current (width : int) (breakpoints : 'a t) : 'a =
  let upper, other = breakpoints in
  let rest = List.sort (fun (a, _) (b, _) -> compare b a) other in
  let rec aux acc = function
    | [] -> acc
    | [(i, v)] -> if width <= i then v else acc
    | (i1, v1) :: (i2, v2) :: tl ->
        if width > i2 && width <= i1 then v1 else aux acc ((i2, v2) :: tl)
  in
  aux upper rest

let to_string (f : 'a -> string) (t : 'a t) : string =
  List.map (fun (k, v) -> Printf.sprintf "[%d, \"%s\"]" k (f v)) (snd t)
  |> String.concat ", "
  |> Printf.sprintf "[%s, [%s]]" (f (fst t))

(* let of_string (f : string -> 'a) (s : string) : 'a t =
 *   let open CCParse in
 *   let string = string "\"" *> U.word <* string "\"" in
 *   let pair a b = U.pair ~start:"[" ~stop:"]" a b in
 *   let l = U.list ~sep:"," (pair U.int string) in
 *   let u, o = parse_string_exn (pair U.word l) s in
 *   f u, List.map (fun (k, v) -> k, f v) o *)
