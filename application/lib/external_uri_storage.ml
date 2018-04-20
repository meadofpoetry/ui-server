open Containers
open Application_types

let compare l r = match l, r with
  | (`Input (li, lid)), (`Input (ri, rid)) ->
     let c = Common.Topology.input_compare li ri in
     if c <> 0 then c
     else compare lid rid
  | (`Board l), (`Board r) -> compare l r
  | (`Board _), (`Input _) -> -1
  | (`Input _), (`Board _) -> 1

module Map = Map.Make(struct type t = marker let compare = compare end)
           
type t = (Common.Url.t * Common.Stream.t) list Map.t

type lst = (marker * (Common.Url.t * Common.Stream.t) list) list [@@deriving yojson]

let default = Map.empty

let dump c = Yojson.Safe.to_string @@ lst_to_yojson @@ Map.to_list c

let restore s =
  let open Result in
  Yojson.Safe.from_string s
  |> lst_of_yojson
  >|= Map.of_list
