open Containers
open Common

let ( % )   = Fun.( % )
let ( >|= ) = Lwt.Infix.(>|=)
let ( >>= ) = Api.Interaction.Json.( >>= )
let ( %> )  = Fun.( %> )

let socket_table : (int,unit React.event) Hashtbl.t = Hashtbl.create 1000

let stream_assoc_to_yojson _to =
  Json.(List.to_yojson (Pair.to_yojson Stream.ID.to_yojson _to))
