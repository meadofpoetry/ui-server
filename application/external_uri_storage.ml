module Map = Map.Make(struct
                 type t = Application_types.Stream.marker
                 let compare = Application_types.Stream.marker_compare
               end)
           
type t = Application_types.Stream.Table.setting list Map.t

type lst = (Application_types.Stream.marker
            * Application_types.Stream.Table.setting list) list [@@deriving yojson]
         
let default = Map.empty

let equal_settings l r =
  let open Application_types.Stream.Table in
  let sort_uniq = List.sort_uniq compare_setting in
  let l = sort_uniq l in
  let r = sort_uniq r in
  try List.iter2 (fun l r -> if not (equal_setting l r) then raise_notrace Exit) l r;
      true
  with _ -> false
            
let equal : t -> t -> bool = Map.equal equal_settings
            
let to_string c = Yojson.Safe.to_string @@ lst_to_yojson @@ List.of_seq @@ Map.to_seq c

let of_string s =
  let open Result in
  Yojson.Safe.from_string s
  |> lst_of_yojson
  |> function
    | Error e -> failwith e
    | Ok lst ->
       Map.of_seq @@ List.to_seq lst
