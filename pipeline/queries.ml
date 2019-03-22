open Netlib.Uri
(* TODO remove *)
module Data_spec = struct
  open Query

  let to_query ?stream ?channel ?pid () : Query.t =
      make [ "stream", (module Option(Int))
           ; "channel", (module Option(Int))
           ; "pid", (module Option(Int))]
        stream channel pid
(*
  let with_query q f ef =
    parse_query [ "stream", (module Option(Int))
                ; "channel", (module Option(Int))
                ; "pid", (module Option(Int))]
      (fun s c p -> s,c,p) q
    |> function
      | Ok (stream,channel,pid) -> f ?stream ?channel ?pid ()
      | Error e -> ef e
 *)
end
