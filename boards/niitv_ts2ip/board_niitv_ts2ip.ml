open Application_types
open Board_niitv_ts2ip_types
open Board_niitv_ts2ip_protocol
open Util_react
open Boards

let ( >>= ) = Lwt_result.( >>= )

module Config = Kv_v.RW(Board_settings)

(* let get_ports_sync board streams =
 *   let open React in
 *   List.fold_left (fun acc p ->
 *       S.map ~eq:(=) (fun streams ->
 *           List.filter_map (Stream.to_topo_port board) streams
 *           |> List.mem ~eq:Topology.equal_topo_port p) streams
 *       |> fun x -> Board.Ports.add p.port x acc)
 *     Board.Ports.empty board.ports *)

(* let of_packer_setting ~present (ps : packer_settings) =
 *   let open Stream.Table in
 *   let (uri :url) =
 *     { ip = ps.dst_ip
 *     ; port = ps.dst_port
 *     } in
 *   { url = Some uri
 *   ; present
 *   ; stream = ps.stream
 *   }
 * 
 * let find_and_rest (f : 'a -> bool) (l : 'a list) =
 *   let rec aux acc = function
 *     | [] -> None, List.rev acc
 *     | hd :: tl ->
 *        if f hd
 *        then Some hd, (List.rev acc) @ tl
 *        else aux (hd :: acc) tl
 *   in
 *   aux [] l
 * 
 * let make_streams (events : Board_protocol.events) =
 *   React.S.l2 ~eq:(Equal.list Stream.Table.equal_stream)
 *     (fun incoming config ->
 *       let merged, rest =
 *         List.fold_left (fun (acc, rest) (s : Stream.t) ->
 *             let opt, rest =
 *               find_and_rest (fun (c : packer_settings) ->
 *                   Stream.equal s c.stream) rest in
 *             let res = match opt with
 *               | Some (ps : packer_settings) ->
 *                  of_packer_setting ~present:true ps
 *               | None ->
 *                  { url = None
 *                  ; present = true
 *                  ; stream = s } in
 *             (res :: acc), rest)
 *           ([], config.packers) incoming in
 *       let rest = List.map (of_packer_setting ~present:false) rest in
 *       merged @ rest)
 *     events.in_streams events.config *)

let create (b : Topology.topo_board)
    (streams : Stream.t list React.signal)
    (convert_streams : Topology.topo_board ->
     Stream.Raw.t list React.signal ->
     Stream.t list React.signal)
    (send : Cstruct.t -> unit Lwt.t)
    (db : Db.t)
    (kv : Kv.RW.t) : (Board.t, [> Board.error]) Lwt_result.t =
  Lwt.return @@ Boards.Board.create_log_src b
  >>= fun (src : Logs.src) ->
  let default = Board_settings.default in
  Config.create ~default kv ["board"; (string_of_int b.control)]
  >>= fun (cfg : config Kv_v.rw) ->
  Protocol.create src send streams (convert_streams b) cfg b.ports b.control
  >>= fun (api : Protocol.api) ->
  let state = object
    method finalize () = Lwt.return ()
  end in
  (* let state =
   *   React.S.l2 ~eq:Stream.Table.equal_source_state
   *     (fun s d ->
   *       match s, d with
   *       | `Fine, Some devi ->
   *          begin match devi.packers_num with
   *          | Some x when x > 0 -> `Limited x
   *          | Some _ | None -> `Forbidden
   *          end
   *       | _ -> `Forbidden)
   *     events.state events.devinfo in
   * let (range : (Stream.Table.url * Stream.Table.url) list) =
   *   [ { ip = Ipaddr.V4.make 224 1 2 2; port = 1234 },
   *     { ip = Ipaddr.V4.make 239 255 255 255; port = 65535 }
   *   ] in
   * let constraints = Board.{ state; range } in
   * let set streams =
   *   match React.S.value events.state with
   *   | `Fine ->
   *      (try
   *         List.map (fun ({ url; stream } : Stream.Table.setting) ->
   *             if List.fold_left (fun acc x ->
   *                    if not @@ Url.in_range x url
   *                    then false else acc) true constraints.range
   *             then { stream
   *                  ; dst_ip = url.ip
   *                  ; dst_port = url.port
   *                  ; enabled = true
   *                  }
   *             else failwith "not in range") streams
   *         |> api.set_packers
   *         |> Lwt_result.map_err (function
   *                | `Limit_exceeded x -> `Limit_exceeded x
   *                | `Undefined_limit  -> `Forbidden)
   *       with _ -> Lwt_result.fail `Not_in_range)
   *   | _ -> Lwt_result.fail `Forbidden
   * in *)
  let board =
    { Board.
      http = Board_niitv_ts2ip_http.handlers b.control api
    ; ws = Board_niitv_ts2ip_http.ws b.control api
    ; templates = []
    ; control = b.control
    ; streams_signal = api.notifs.outgoing_streams
    ; log_source = (fun _ -> React.E.never) (* TODO implement source *)
    ; loop = api.loop
    ; push_data = api.push_data
    ; connection = api.notifs.state
    ; ports_sync =
        List.fold_left (fun acc (p : Topology.topo_port) ->
            Board.Ports.add p.port (React.S.const false) acc)
          Board.Ports.empty b.ports
    ; ports_active =
        List.fold_left (fun acc (p : Topology.topo_port) ->
            Board.Ports.add p.port (React.S.const true) acc)
          Board.Ports.empty b.ports
    ; stream_handler = None
    (* Some (object
     *   method streams = make_streams events
     *   method set x = set x
     *   method constraints = constraints
     * end) *)
    ; state = (state :> < finalize : unit -> unit Lwt.t >)
    } in
  Lwt.return_ok board
