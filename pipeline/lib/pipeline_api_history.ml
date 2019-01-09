open Containers
open Pipeline_api_common
open Api.Interaction
open Api.Interaction.Json
open Common

module HTTP = struct

  type res = (Stream.t * Time.t * Time.t) list [@@deriving yojson]

  type res_struct = (Structure.t * Time.t) list [@@deriving yojson]

  let get_streams db limit from till duration _ _ () =
    match Time.make_interval ?from ?till ?duration () with
    | Ok `Range (from,till) ->
       Db.Streams.select_streams db ?limit ~from ~till
       |> Lwt_result.map (Api.Api_types.rows_to_yojson res_to_yojson (fun () -> `Null))
       |> Lwt_result.map_err (fun e -> `String e)
       >>= respond_result
    | _ -> respond_error ~status:`Not_implemented "FIXME" ()

  let get_structures db uris limit from till duration _ _ () =
    let uris = List.map Stream.tsoip_id_of_url uris in
    match Time.make_interval ?from ?till ?duration () with
    | Ok `Range (from,till) ->
       Db.Structure.select_structures db ?limit ~uris ~from ~till
       |> Lwt_result.map (Api.Api_types.rows_to_yojson res_struct_to_yojson (fun () -> `Null))
       |> Lwt_result.map_err (fun e -> `String e)
       >>= respond_result
    | _ -> respond_error ~status:`Not_implemented "FIXME" ()

end

let handler (api : Pipeline_protocol.api) =
  let open Uri in
  let open Api_handler in
  create_dispatcher
    "history"
    []
    [ `GET,
      [ create_handler ~docstring:"Streams archive"
          ~path:Path.Format.("streams/archive" @/ empty)
          ~query:Query.[ "limit",    (module Option(Int))
                       ; "from",     (module Option(Time.Show))
                       ; "to",       (module Option(Time.Show))
                       ; "duration", (module Option(Time.Relative)) ]
          (HTTP.get_streams api.model.db)
      ; create_handler ~docstring:"Structure archive"
          ~path:Path.Format.("structure/archive" @/ empty)
          ~query:Query.[ "uris",     (module List(Url.Q))
                       ; "limit",    (module Option(Int))
                       ; "from",     (module Option(Time.Show))
                       ; "to",       (module Option(Time.Show))
                       ; "duration", (module Option(Time.Relative)) ]
          (HTTP.get_structures api.model.db)
      ]
    ]
