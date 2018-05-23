open Containers
open Board_types
open Types

let prioriry_of_err_code = function
  | x when x >= 0x11 && x <= 0x16 -> Some 1
  | x when x >= 0x21 && x <= 0x26 -> Some 2
  | x when x >= 0x31 && x <= 0x38 -> Some 3
  | _                             -> None

let map_ts_errors (streams:Common.Stream.t list) (e:ts_errors_raw) : ts_error list =
  List.find_opt (fun (s:Common.Stream.t) ->
      match s.id with
      | `Ts id -> Common.Stream.equal_id id e.stream_id
      | _      -> false) streams
  |> Option.map (fun s ->
         List.filter_map (fun (x:ts_error_raw) ->
             Option.map (fun p -> { stream    = s
                                  ; timestamp = e.timestamp
                                  ; count     = x.count
                                  ; err_code  = x.err_code
                                  ; err_ext   = x.err_ext
                                  ; priority  = p
                                  ; multi_pid = x.multi_pid
                                  ; pid       = x.pid
                                  ; packet    = x.packet
                                  ; param_1   = x.param_1
                                  ; param_2   = x.param_2 })
             @@ prioriry_of_err_code x.err_code) e.errors)
  |> function None -> [] | Some l -> l
