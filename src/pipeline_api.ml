open Api_handler

let (>>=) = Lwt.(>>=)

let test _ _ body =
  Cohttp_lwt_body.to_string body >>= fun body ->
  let jss = String.split_on_char '=' body |> fun l -> List.nth l 1 in
  let js  = Uri.pct_decode jss |> Yojson.Safe.from_string in
  Lwt_io.printf "Got: %s\n" (Yojson.Safe.to_string js) >>= fun _ ->
  let s =
    Qoe_types.State.of_yojson js
    |> function
      | Error _ -> "Sorry, something is wrong with your json"
      | Ok root -> Lwt_io.printf "Msgpck: %s\n" (Qoe.Qoe_types.State.to_msgpck root) |> ignore;
                   let prefix = "Thank you, master! " in
                   begin match (CCOpt.get_exn root.graph).state with
                   | Some Null  -> prefix ^ "You want me to stop the graph?"
                   | Some Stop  -> prefix ^ "Halting graph"
                   | Some Play  -> prefix ^ "Starting"
                   | Some Pause -> prefix ^ "Graph is going to be paused"
                   | None       -> prefix ^ "I don't understand"
                   end
  in
  Cohttp_lwt_unix.Server.respond_string ~status:`OK ~body:s ()

let basic_handle _ id meth args headers body =
  match meth, args with
  | `POST, _ -> test id headers body
  | _ -> Redirect.not_found ()
  
let handlers pipe =
  [ (module struct
       let domain = "test"
       let handle = basic_handle pipe
     end : HANDLER); ]
