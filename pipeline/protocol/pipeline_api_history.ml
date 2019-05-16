open Application_types
open Pipeline_types
open Lwt.Infix

type res = (Stream.t * Time.t * Time.t) list [@@deriving yojson]

type res_struct = (Structure.t * Time.t) list [@@deriving yojson]

module Interval = Time.Interval.Make(Ptime_clock)

(* TODO remove 4.08 *)

let filter_map f l =
  let rec loop acc = function
    | [] -> List.rev acc
    | x::tl ->
       match f x with
       | None -> loop acc tl
       | Some v -> loop (v::acc) tl
  in loop [] l

let get_streams (state : Protocol.state)
      limit from till duration _user _body _env _state =
  match Interval.make ?from ?till ?duration () with
  | Ok `Range (from,till) ->
     Database.Streams.select_streams state.model.db ?limit ~from ~till
     |> Lwt_result.map (Api.rows_to_yojson res_to_yojson (fun () -> `Null))
     >>= (function
          | Ok v -> Lwt.return (`Value v)
          | Error e -> Lwt.return (`Error e))
  | _ -> Lwt.return `Not_implemented

let get_structures (state : Protocol.state)
      uris limit from till duration _user _body _env _state =
  let uris = filter_map Stream.tsoip_id_of_uri uris in
  match Interval.make ?from ?till ?duration () with
  | Ok `Range (from,till) ->
     Database.Structure.select_structures state.model.db ?limit ~uris ~from ~till
     |> Lwt_result.map (Api.rows_to_yojson res_struct_to_yojson (fun () -> `Null))
     >>= (function
          | Ok v -> Lwt.return (`Value v)
          | Error e -> Lwt.return (`Error e))
  | _ -> Lwt.return `Not_implemented
