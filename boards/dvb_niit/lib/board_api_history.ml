open Api.Interaction
open Api.Interaction.Json
open Board_api_common
open Board_types
open Common

module HTTP = struct

  module Measurements = struct

    let raw_to_yojson (data : (Stream.ID.t * Measure.t Time.timestamped list) list)
        : Yojson.Safe.json =
      let open Json in
      List.to_yojson
        (Pair.to_yojson
           Stream.ID.to_yojson
           (List.to_yojson @@ Time.timestamped_to_yojson Measure.to_yojson))
          data

    let get db streams tuners limit from till duration _ _ () =
      match Time.make_interval ?from ?till ?duration () with
      | Ok `Range (from, till) ->
         Db.Measurements.select ?limit ~tuners ~streams ~from ~till db ()
         >>= fun v ->
         let r = Ok (Api.Api_types.rows_to_yojson
                       raw_to_yojson
                       (fun _ -> assert false)
                       v) in
         respond_result r
      | Error e -> respond_error e ()
      | _ -> respond_error ~status:`Not_implemented "FIXME" ()

  end

end

let handler db =
  let open Uri in
  let open Boards.Board.Api_handler in
  create_dispatcher
    "history"
    []
    [ `GET,
      [ create_handler ~docstring:"Returns measurements"
          ~path:Path.Format.("measurements" @/ empty)
          ~query:Query.[ "id", (module List(Stream.ID))
                       ; "tuner", (module List(Int))
                       ; "limit", (module Option(Int))
                       ; "from", (module Option(Time.Show))
                       ; "to", (module Option(Time.Show))
                       ; "duration", (module Option(Time.Relative)) ]
          (HTTP.Measurements.get db)
      ]
    ]
