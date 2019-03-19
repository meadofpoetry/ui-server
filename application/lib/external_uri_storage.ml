module Map = Map.Make(struct
                 type t = Application_types.Stream.marker
                 let compare = Application_types.Stream.marker_compare
               end)
           
type t = Application_types.Stream.Table.setting list Map.t

type lst = (Application_types.Stream.marker
            * Application_types.Stream.Table.setting list) list [@@deriving yojson]

let default = Map.empty

let to_string c = Yojson.Safe.to_string @@ lst_to_yojson @@ List.of_seq @@ Map.to_seq c

let of_string s =
  let open Result in
  Yojson.Safe.from_string s
  |> lst_of_yojson
  |> function
    | Error e -> failwith e
    | Ok lst ->
       Map.of_seq @@ List.to_seq lst
