open Containers
open Lwt.Infix
   
type t = { network : Network.t }
               
let create config =
  let open Result in
  Network.create config >>= fun network ->
  Ok { network }

let finalize o =
  Network.finalize o.network
