open Board_dvb_types

let ( >>= ) = Lwt.bind

type t =
  { db : Database.Conn.t
  ; tick : unit React.event
  ; loop : unit Lwt.t
  ; measures : unit React.event
  }

let tick () =
  let e, push = React.E.create () in
  let rec loop () =
    Lwt_unix.sleep 5. >>= fun () -> push (); loop () in
  e, loop

let create (log_src : Logs.src)
      (control : int)
      (measures : (int * Measure.t ts) list React.event)
      (db : Db.state) =
  let ( >>= ) = Lwt_result.bind in
  Database.Conn.create db control
  >>= fun db ->
  let tick, loop = tick () in
  let measures =
    Util_react.E.map_s (fun x ->
        Lwt.catch (fun () -> Database.Measurements.insert db x)
          (function
           | Failure e ->
              Logs_lwt.err ~src:log_src (fun m ->
                  m "measures db error: %s" e)
           | exn ->
              Logs_lwt.err ~src:log_src (fun m ->
                  let s = Printexc.to_string exn in
                  m "measures db error: %s" s)))
      measures in
  Lwt.return_ok { db; tick; loop = loop (); measures }
