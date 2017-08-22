let (<+>) a b =
  match a,b with
  | _, Some _ -> b
  | Some _, None -> a
  | _ -> None

let opt_update f a b =
  match a,b with
  | Some a, Some b -> Some (f a b)
  | Some x, None | None, Some x -> Some x
  | _ -> None

let rec filter_none : Yojson.Safe.json -> Yojson.Safe.json = function
  | `Assoc tl  -> let vk = List.filter (function (_, `Null) -> false | _ -> true) tl
                  in `Assoc (List.map (fun (s,o) -> s, filter_none o) vk)
  | `List lst  -> `List (List.map filter_none lst)
  | `Tuple lst -> `Tuple (List.map filter_none lst)
  | `Variant (s, Some o) -> `Variant (s,Some (filter_none o))
  | o -> o
