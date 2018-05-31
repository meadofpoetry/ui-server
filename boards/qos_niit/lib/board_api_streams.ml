open Containers
open Board_types
open Board_protocol
open Board_api_common
open Api.Interaction
open Api.Redirect
open Common.Uri.Query

(**
 ** API
 **
 ** GET  /streams/{?stream}
 ** GET  /streams/state/[ts|t2mi]/{?stream}
 ** GET  /streams/structure/[ts|t2mi]/{?stream}
 ** GET  /streams/bitrate/ts/{?stream}
 ** GET  /streams/sequence/t2mi/
 ** GET  /streams/section/ts/{stream}/{table-id}
 **
 **)

include Api_utils.Streams

module WS = struct

  open Common

  let streams sock_data (events:events) body () =
    sock_handler sock_data (React.S.changes events.streams) Stream.t_list_to_yojson body

  let fmap ~(eq:'b -> 'b -> bool) f_id f_time stream (e:'a list React.event) =
    React.E.fmap (fun x ->
        List.filter (fun s -> eq stream (f_id s)) x
        |> (function
            | [] -> None
            | l  -> List.sort (fun x y -> Time.compare (f_time x) (f_time y)) l
                    |> List.rev |> List.head_opt)) e

  module TS = struct

    open Board_types.Streams.TS

    let fmap f_id f_time stream e = fmap ~eq:Common.Stream.equal_id f_id f_time stream e

    let state ?stream sock_data (ev:events) body () =
      let f e j = sock_handler sock_data e j body in
      let e     = ev.ts_states in
      match stream with
      | Some id -> let e = fmap (fun (x:state) -> x.stream) (fun x -> x.timestamp) id e in
                   f e state_to_yojson
      | None    -> f e states_to_yojson

    let bitrate ?stream sock_data (ev:events) body () =
      let f e j = sock_handler sock_data e j body in
      let e     = React.S.changes ev.bitrates in
      match stream with
      | Some id -> let e = fmap (fun x -> x.stream) (fun x -> x.timestamp) id e in
                   f e structure_to_yojson
      | None    -> f e structures_to_yojson

    let structure ?stream sock_data (ev:events) body () =
      let f e j = sock_handler sock_data e j body in
      let e     = React.S.changes ev.structs in
      match stream with
      | Some id -> let e = fmap (fun x -> x.stream) (fun x -> x.timestamp) id e in
                   f e structure_to_yojson
      | None    -> f e structures_to_yojson


  end

  module T2MI = struct

    open Board_types.Streams.T2MI

    let fmap f_id f_time stream e = fmap ~eq:Int.equal f_id f_time stream e

    let state ?stream sock_data (ev:events) body () =
      let f e j = sock_handler sock_data e j body in
      let e     = ev.t2mi_states in
      match stream with
      | Some id -> let e = fmap (fun (x:state) -> x.stream_id) (fun x -> x.timestamp) id e in
                   f e state_to_yojson
      | None    -> f e states_to_yojson

    let structure ?stream sock_data (ev:events) body () =
      let f e j = sock_handler sock_data e j body in
      let e     = React.S.changes ev.t2mi_info in
      match stream with
      | Some id -> let e = fmap (fun (x:structure) -> x.stream_id) (fun x -> x.timestamp) id e in
                   f e structure_to_yojson
      | None    -> f e structures_to_yojson

  end

end

module REST = struct

  (** Real-time GET requests **)
  module RT = struct

    let to_json ?stream s =
      (match stream with
       | Some id -> List.find_opt (fun (x:Common.Stream.t) -> match x.id with
                                                              | `Ts x -> Common.Stream.equal_id id x
                                                              | _     -> false) s
                    |> Common.Stream.t_opt_to_yojson
       | None    -> Common.Stream.t_list_to_yojson s)
      |> Result.return
      |> Json.respond_result

    let streams ?stream (ev:events) () =
      let s = React.S.value ev.streams in to_json ?stream s

    module TS = struct

      open Board_types.Streams.TS

      let to_json ?stream s =
        (match stream with
         | Some id -> List.find_opt (fun (x:structure) -> Common.Stream.equal_id id x.stream) s
                      |> structure_response_to_yojson
         | None    -> structures_to_yojson s)
        |> Result.return
        |> Json.respond_result

      let structure ?stream (ev:events) () =
        let v = React.S.value ev.structs in to_json ?stream v

      let bitrate ?stream (ev:events) () =
        let v = React.S.value ev.bitrates in to_json ?stream v

      let si_psi_section stream_id table_id (api:api) (q:Raw.t list) () =
        let r,_ = Validation.(
            get (One ("section",Int)) q
            >>= fun (sct,q) -> get (One ("table-id-ext",Int)) q
            >>= fun (ext,q) -> get (One ("eit-ts-id",Int)) q
            >>= fun (sid,q) -> get (One ("eit-orig-nw-id",Int)) q
            >>| fun nid     -> sct,ext,sid,nid)
        in (fun (section,table_id_ext,eit_ts_id,eit_orig_nw_id) ->
            let req = { stream_id; table_id; section; table_id_ext; eit_ts_id; eit_orig_nw_id } in
            api.get_section req >|= (function
                                     | Ok x    -> Ok (section_to_yojson x)
                                     | Error e -> Error (section_error_to_yojson e))
            >>= Json.respond_result)
           |> query_wrapper r

    end

    module T2MI = struct

      open Board_types.Streams.T2MI

      let structure ?stream (e:events) () =
        let v = React.S.value e.t2mi_info in
        (match stream with
         | Some id -> List.find_opt (fun (x:structure) -> Int.equal id x.stream_id) v
                      |> structure_response_to_yojson
         | None    -> structures_to_yojson v)
        |> Result.return
        |> Json.respond_result

      let sequence ?stream (api:api) (q:Raw.t list) () =
        let r = Validation.(last_or_err @@ get_or ~default:5 (One ("seconds",Int)) q) in
        (fun seconds ->
          api.get_t2mi_seq seconds
          >|= (fun x -> let seq = match stream with
                          | Some id -> List.filter (fun (x:sequence_item) -> id = x.stream_id) x
                          | None    -> x
                        in Ok (sequence_to_yojson seq))
          >>= Json.respond_result)
        |> query_wrapper r

    end

  end

  (** Archive GET requests **)
  module AR = struct

    let streams ?stream (q:Raw.t list) time () =
      let r,_ = Validation.(
          get_limit_query q
          >>= fun (lim,q) -> get_total_query q
          >>| fun tot     -> lim,tot)
      in (fun (lim,tot) -> (* TODO IMPLEMENT *)
          respond_error ~status:`Not_implemented "not impelemented" ())
         |> query_wrapper r

    module TS = struct

      open Board_types.Streams.TS

      let state ?stream (q:Raw.t list) time () =
        let r,_ = Validation.(
            get_state_query q
            >>= fun (fil,q) -> get_limit_query q
            >>= fun (lim,q) -> get_thin_query q
            >>= fun (thn,q) -> get_total_query q
            >>| fun tot     -> fil,lim,thn,tot)
        in (fun (filter,limit,thin,total) -> (* TODO IMPLEMENT *)
            respond_error ~status:`Not_implemented "not impelemented" ())
           |> query_wrapper r

      let structure ?stream (q:Raw.t list) time () =
        let r,_ = Validation.(
            get_limit_query q
            >>= fun (lim,q) -> get_total_query q
            >>| fun tot     -> lim,tot)
        in (fun (limit,total) -> (* TODO IMPLEMENT *)
            respond_error ~status:`Not_implemented "not impelemented" ())
           |> query_wrapper r

      let bitrate ?stream (q:Raw.t list) time () =
        let r,_ = Validation.(
            get_limit_query q
            >>= fun (lim,q) -> get_thin_query q
            >>= fun (thn,q) -> get_total_query q
            >>| fun tot     -> lim,thn,tot)
        in (fun (limit,thin,total) -> (* TODO IMPLEMENT *)
            respond_error ~status:`Not_implemented "not impelemented" ())
           |> query_wrapper r

    end

    module T2MI = struct

      open Board_types.Streams.T2MI

      let state ?stream (q:Raw.t list) time () =
        let r,_ = Validation.(
            get_state_query q
            >>= fun (fil,q) -> get_limit_query q
            >>= fun (lim,q) -> get_thin_query q
            >>= fun (thn,q) -> get_total_query q
            >>| fun tot     -> fil,lim,thn,tot)
        in (fun (filter,limit,thin,total) -> (* TODO IMPLEMENT *)
            respond_error ~status:`Not_implemented "not impelemented" ())
           |> query_wrapper r

      let structure ?stream (q:Raw.t list) time () =
        let r,_ = Validation.(
            get_limit_query q
            >>= fun (lim,q) -> get_total_query q
            >>| fun tot     -> lim,tot)
        in (fun (limit,total) -> (* TODO IMPLEMENT *)
            respond_error ~status:`Not_implemented "not impelemented" ())
           |> query_wrapper r

    end

  end

end

let handle_ok api events scheme meth req (q:Raw.t list) sock_data body time () =
  match scheme,meth,req,time with
  (* Websockets *)
  | `WS,`GET,`Streams id,            `Now    -> WS.streams sock_data events body ()
  | `WS,`GET,`State (`TS id),        `Now    -> WS.TS.state ?stream:id sock_data events body ()
  | `WS,`GET,`Bitrate id,            `Now    -> WS.TS.bitrate ?stream:id sock_data events body ()
  | `WS,`GET,`Structure (`TS id),    `Now    -> WS.TS.structure ?stream:id sock_data events body ()
  | `WS,`GET,`State (`T2MI id),      `Now    -> WS.T2MI.state ?stream:id sock_data events body ()
  | `WS,`GET,`Structure (`T2MI id),  `Now    -> WS.T2MI.structure ?stream:id sock_data events body ()
  (* REST *)
  | `REST,`GET,`Streams id,          `Now    -> REST.RT.streams ?stream:id events ()
  | `REST,`GET,`Structure (`TS id),  `Now    -> REST.RT.TS.structure ?stream:id events ()
  | `REST,`GET,`Bitrate id,          `Now    -> REST.RT.TS.bitrate ?stream:id events ()
  | `REST,`GET,`Section (sid,id),    `Now    -> REST.RT.TS.si_psi_section sid id api q ()
  | `REST,`GET,`Structure (`T2MI id),`Now    -> REST.RT.T2MI.structure ?stream:id events ()
  | `REST,`GET,`Sequence id,         `Now    -> REST.RT.T2MI.sequence ?stream:id api q ()
  | `REST,`GET,`Streams id,          `Past t -> REST.AR.streams ?stream:id q t ()
  | `REST,`GET,`State (`TS id),      `Past t -> REST.AR.TS.state ?stream:id q t ()
  | `REST,`GET,`Structure (`TS id),  `Past t -> REST.AR.TS.structure ?stream:id q t ()
  | `REST,`GET,`Bitrate id,          `Past t -> REST.AR.TS.bitrate ?stream:id q t ()
  | `REST,`GET,`State (`T2MI id),    `Past t -> REST.AR.T2MI.state ?stream:id q t ()
  | `REST,`GET,`Structure (`T2MI id),`Past t -> REST.AR.T2MI.structure ?stream:id q t ()
  | _ -> not_found ()

let handle api events scheme meth req uri sock_data body () =
  match get_time_query @@ Uri.query uri with
  | Error e,_ -> Json.respond_result (Error (Api_utils.err_to_yojson @@ Bad_query e))
  | Ok t,q    -> handle_ok api events scheme meth req q sock_data body t ()
