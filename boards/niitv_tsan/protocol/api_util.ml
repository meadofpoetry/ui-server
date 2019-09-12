open Application_types
open Board_niitv_tsan_types

let ( % ) f g x = f (g x)

let not_responding () = Lwt.return_error Request.Not_responding

let return_value x = Lwt.return (`Value x)

let return_error e = Lwt.return (`Error (Request.error_to_string e))

let map_stream_id streams =
  List.filter_map (fun (id, v) ->
      match Stream.find_by_multi_id id streams with
      | None -> None
      | Some s -> Some (s.id, v))

let ( >>= ) = Lwt.( >>= )

let ( >>=? ) x f =
  x
  >>= function
  | Ok x -> f x
  | Error e -> return_error e

let int_ms_to_float_s (x : int) = float_of_int x /. 1000.

let stream_pair_to_yojson f = Util_json.Pair.to_yojson Stream.ID.to_yojson f

let stream_assoc_list_to_yojson f = Util_json.List.to_yojson @@ stream_pair_to_yojson f

let find_by_id id l =
  match List.find_opt (Stream.ID.equal id % fst) l with
  | None -> None
  | Some (_, x) -> Some x

let find_map_by_id id f l =
  match find_by_id id l with
  | None -> None
  | Some x -> Some (f x)

let filter_streams ids v =
  match ids with
  | [] -> v
  | ids -> List.filter (fun (s : Stream.t) -> List.exists (Stream.ID.equal s.id) ids) v

let filter_ids ids v =
  match ids with
  | [] -> v
  | ids -> List.filter (fun (id, _) -> List.exists (Stream.ID.equal id) ids) v

let check_state (state : Application_types.Topology.state React.signal) =
  match React.S.value state with
  | `Fine -> Lwt.return_ok ()
  | `No_response | `Init | `Detect -> Lwt.return_error Request.Not_responding

let pids_to_yojson =
  Util_json.(List.to_yojson @@ Pair.to_yojson Int.to_yojson PID.to_yojson)

let pids_ts_to_yojson = ts_to_yojson pids_to_yojson

let si_psi_tables_to_yojson =
  Util_json.(
    List.to_yojson @@ Pair.to_yojson SI_PSI_table.id_to_yojson SI_PSI_table.to_yojson)

let services_to_yojson =
  Util_json.(List.to_yojson @@ Pair.to_yojson Int.to_yojson Service.to_yojson)

let services_ts_to_yojson = ts_to_yojson services_to_yojson

let t2mi_info_to_yojson =
  Util_json.(List.to_yojson @@ Pair.to_yojson Int.to_yojson T2mi_info.to_yojson)
