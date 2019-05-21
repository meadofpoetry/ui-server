open Board_niitv_dvb_types
open Api_util

module Interval = Time.Interval.Make(Ptime_clock)

type res = (int * Measure.t ts list) list [@@deriving yojson]

let get_measurements (api : Protocol.api) ids limit from till duration
    _user _body _env _state =
  match Interval.make ?from ?till ?duration () with
  | Ok `Range (from, till) -> begin
      let tuners = match ids with [] -> None | l -> Some l in
      Database.Measurements.select api.model.db ?tuners ?limit ~from ~till ()
      |> Lwt_result.map (Api.rows_to_yojson res_to_yojson (fun () -> `Null))
      >>= function
      | Ok v -> return_value v
      | Error e -> Lwt.return (`Error e)
    end
  | _ -> Lwt.return `Not_implemented
