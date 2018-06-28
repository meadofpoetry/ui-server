open Containers

let ( % )   = Fun.( % )
let ( >|= ) = Lwt.Infix.(>|=)
let ( >>= ) = Api.Interaction.Json.( >>= )
let ( %> )  = Fun.( %> )

let socket_table : (int,unit React.event) Hashtbl.t = Hashtbl.create 1000
