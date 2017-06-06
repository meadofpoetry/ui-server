open Containers

let (>>=) = Lwt.(>>=)

let test _ _ _ _ body =
  let open Common in
  Cohttp_lwt_body.to_string body >>= fun body ->
  let jss = String.split_on_char '=' body |> fun l -> List.nth l 1 in
  (*let js = Yojson.Safe.from_string jss in*)
  Lwt_io.printf "Test body: %s\n" jss >>= fun _ ->
  Cohttp_lwt_unix.Server.respond_string ~status:`OK ~body:"Thank you, master!" ()
