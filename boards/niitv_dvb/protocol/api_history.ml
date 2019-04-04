open Board_niitv_dvb_types

let ( >>= ) = Lwt.bind

type res = (int * Measure.t ts list) list [@@deriving yojson]

let get_measurements (api : Protocol.api) ids limit from till duration
      _user _body _env _state =
  match Time.make_interval ?from ?till ?duration () with
  | Ok `Range (from, till) ->
     let tuners = match ids with [] -> None | l -> Some l in
     Database.Measurements.select api.model.db ?tuners ?limit ~from ~till ()
     |> Lwt_result.map (Api.rows_to_yojson res_to_yojson (fun () -> `Null))
     >>= (function
          | Ok v -> Lwt.return (`Value v)
          | Error e -> Lwt.return (`Error e))
  | _ -> Lwt.return `Not_implemented
