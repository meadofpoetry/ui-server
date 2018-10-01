open Containers
open Qoe_errors
open Common

open Lwt.Infix

type t = { db : Db.Conn.t
         ; tick  : unit Lwt_react.event
         ; _loop : unit Lwt.t
         }

let appeared_pids ~past ~pres =
  let open Structure in
  let flat (sl : structure list) =
    List.fold_left (fun acc s ->
        let stream = Int32.to_int s.id in
        let l = List.fold_left (fun acc c ->
                    let channel = c.number in
                    let l = List.fold_left (fun acc p -> (stream, channel, p.pid, p.to_be_analyzed)::acc)
                              [] c.pids in
                    l @ acc)
                  [] s.channels in
        l @ acc) [] sl
  in
  let rec not_in_or_diff (s,c,p,tba) = function
    | [] -> true
    | (so,co,po,tbao)::_
         when so = s && co = c && po = p && Bool.(not @@ equal tbao tba) -> true
    | (so,co,po,tbao)::_
         when so = s && co = c && po = p && Bool.(equal tbao tba) -> false
    | _::tl -> not_in_or_diff (s,c,p,tba) tl
  in                          
  let past = flat past in
  let pres = flat pres in
  let appeared = List.fold_left (fun acc pres ->
                     let (_,_,_,tba) = pres in
                     if tba && not_in_or_diff pres past
                     then pres::acc else acc) [] pres in
  appeared

let active_pids str =
  let open Structure in
  let flat_filter (sl : structure list) =
    List.fold_left (fun acc s ->
        let stream = Int32.to_int s.id in
        let l = List.fold_left (fun acc c ->
                    let channel = c.number in
                    let l = List.fold_left (fun acc p ->
                                if p.to_be_analyzed
                                then (stream, channel, p.pid, p.to_be_analyzed)::acc
                                else acc)
                              [] c.pids in
                    l @ acc)
                  [] s.channels in
        l @ acc) [] sl
  in
  flat_filter str
                            
let tick () =
  let e,push = Lwt_react.E.create () in
  let rec loop () =
    Lwt_unix.sleep 5. >>= fun () -> push (); loop ()
  in
  e, loop
       
let create db_conf s_struct s_status e_video e_audio =
  let db = Result.get_exn @@ Db.Conn.create db_conf () in
  let tick, loop = tick () in
  (* Pids *)
  let strip = let open Structure in List.map (fun s -> s.structure) in
  let pids =
    let open Structure in
    Lwt_react.S.sample (fun () sl -> `Active (active_pids @@ strip sl)) tick s_struct
  in
  let pids_diff =
    let open Structure in
    Lwt_react.S.diff (fun pres past -> `New (appeared_pids ~past:(strip past) ~pres:(strip pres))) s_struct
  in
  Lwt_react.E.keep
  @@ Lwt_react.E.map_s (function
         | `Active pids -> Db.Pid_state.bump db pids
         | `New pids -> Db.Pid_state.init db pids)
  @@ Lwt_react.E.select [pids; pids_diff];
  (* Structures *)
  Lwt_react.S.keep
  @@ Lwt_react.S.map (fun x -> Lwt.catch (fun () -> Db.Structure.insert_structures db x)
                                 (function Failure e -> Lwt_io.printf "str error: %s\n" e)) s_struct;
  (* Stream status *)
  let stream_status =
    let open Qoe_status in
    Lwt_react.S.sample (fun () sl -> `Bump (List.filter (fun s -> not s.playing) sl))
      tick s_status
  in
  let stream_status_diff =
    let open Qoe_status in
    let merge pres past =
      let find s = List.find (fun sold ->
                       s.stream = sold.stream
                       && s.channel = sold.channel
                       && s.pid = sold.pid)
                     past
      in
      List.fold_left (fun acc s ->
          if s.playing then acc
          else try if (find s).playing
                   then s::acc
                   else acc
               with _ -> s::acc)
        [] pres
    in        
    Lwt_react.S.diff (fun pres past -> `Lost (merge pres past)) s_status
  in
  Lwt_react.E.keep
  @@ Lwt_react.E.map (function
         | `Bump pids -> Db.Stream_status.bump db pids
         | `Lost pids -> Db.Stream_status.init db pids)
  @@ Lwt_react.E.select [stream_status; stream_status_diff];
  (* Errors *)
  Lwt_react.E.keep
  @@ Lwt_react.E.map_p (fun x -> Lwt.catch (fun () -> Db.Errors.insert_video db x)
                                  (function Failure e -> Lwt_io.printf "vdata error: %s\n" e)) e_video;
  Lwt_react.E.keep
  @@ Lwt_react.E.map_p (fun x -> Lwt.catch (fun () -> Db.Errors.insert_audio db x)
                                  (function Failure e -> Lwt_io.printf "adata error: %s\n" e)) e_audio;
  { db; tick; _loop = loop () }
  
let set_streams model streams =
  (* Streams *)
  Lwt.ignore_result 
    (Db.Streams.init model.db streams >|= fun () ->
     Lwt_react.E.keep @@ Lwt_react.E.map_s (fun () -> Db.Streams.bump model.db) model.tick);
