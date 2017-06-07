open Containers

let (>>=) = Lwt.(>>=)

let test _ _ _ _ body =
  let open Common in
  Cohttp_lwt_body.to_string body >>= fun body ->
  let jss = String.split_on_char '=' body |> fun l -> List.nth l 1 in
  let js  = Uri.pct_decode jss |> Yojson.Safe.from_string in
  let s =
    Common.Qoe.Qoe_root.of_yojson js
    |> function
      | Error s -> "Sorry, something is wrong with your json"
      | Ok (root : Qoe.Qoe_root.t) -> let prefix = "Thank you, master!\n" in
                   begin match (CCOpt.get_exn root.graph).Qoe.Graph.state with
                   | Some Null  -> prefix ^ "You want me to stop the graph?"
                   | Some Stop  -> prefix ^ "Halting graph"
                   | Some Play  -> prefix ^ "Starting"
                   | Some Pause -> prefix ^ "Graph is going to be paused"
                   | None       -> prefix ^ "I don't understand"
                   end
  in
  Cohttp_lwt_unix.Server.respond_string ~status:`OK ~body:s ()
