(*

open Containers
open Common

module Api_handler = Api.Handler.Make(User)

let ( % )   = Fun.( % )
let ( %> )  = Fun.( %> )
let ( >|= ) = Lwt.Infix.( >|= )
let ( >>= ) = Api.Interaction.Json.( >>= )

let socket_table : (int, unit React.event) Hashtbl.t = Hashtbl.create 1000
 *)
