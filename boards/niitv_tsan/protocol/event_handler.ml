open Application_types
open Board_niitv_tsan_types

module Acc = struct

  type probes =
    { board_errors : Board_error.t list option
    ; bitrate : (Stream.Multi_TS_ID.t * Bitrate.t) list option
    ; ts_structs : (Stream.Multi_TS_ID.t * structure) list option
    ; t2mi_info : (Stream.Multi_TS_ID.t * T2mi_info.t list) list option
    ; jitter : jitter_raw option
    }

  let (probes_empty : probes) =
    { board_errors = None
    ; bitrate = None
    ; ts_structs = None
    ; t2mi_info = None
    ; jitter = None
    }

  let merge_ts_probes ?(acc = []) l =
    List.fold_left (fun acc (id, x) ->
        List.Assoc.set ~eq:Stream.Multi_TS_ID.equal id x acc) acc l

  let merge_t2mi_info ?(acc = []) (id, x) =
    let open T2mi_info in
    let eq = Stream.Multi_TS_ID.equal in
    let f ((id, info) : t) = function
      | None -> Some [(id, info)]
      | Some (o : t list) ->
         let n = List.Assoc.set ~eq:equal_id id info o in
         Some n in
    List.Assoc.update ~eq (f x) id acc

  let cons (p : probes) : probe_response -> probes = function
    | Board_errors x -> { p with board_errors = Some x }
    | Bitrate x ->
       let bitrate = Some (merge_ts_probes ?acc:p.bitrate x) in
       { p with bitrate }
    | Struct x ->
       let ts_structs = Some (merge_ts_probes ?acc:p.ts_structs x) in
       { p with ts_structs  }
    | T2mi_info x ->
       let t2mi_info = Some (merge_t2mi_info ?acc:p.t2mi_info x) in
       { p with t2mi_info }
    | Jitter x -> { p with jitter = Some x }

  type t =
    { group : group option
    ; events : board_event list
    ; probes : probes
    ; parts : (int * int, Board_parser.part list) List.Assoc.t
    ; bytes : Cstruct.t option
    }

  let (empty : t) =
    { group = None
    ; events = []
    ; probes = probes_empty
    ; parts = []
    ; bytes = None }

  let cons_probe (acc : t) (x : probe_response) : t =
    { acc with probes = cons acc.probes x }

end

module Make(Logs : Logs.LOG) : sig
  val partition : Acc.t -> group list * Acc.t
  val get_req_stack : group -> group option -> probe_response probe_request list
  val handle : push_events -> events -> init -> Acc.t -> Acc.t
  val handle_immediate : push_events -> Acc.t -> Acc.t
end = struct

  let jitter_ptr = ref (-1l)

  let split_by l sep =
    let res, acc =
      List.fold_left (fun (res, acc) x ->
          if equal_board_event x sep
          then ((List.rev acc) :: res), []
          else res, (x :: acc))
        ([], []) l
    in (List.rev res), (List.rev acc)

  (** Returns merged groups and accumulator. Last group received is head of list *)
  let partition (acc : Acc.t) =
    let groups, rest = split_by acc.events `End_of_transmission in
    let groups =
      List.filter (function
          | `Status _ :: `Streams_event _ :: _ -> true
          | _ -> false) groups
      |> List.fold_left (fun (gps : group list) (x : board_event list) ->
             let prev_status = match gps with
               | [] -> Option.(acc.group >|= (fun x -> x.status))
               | x :: _ -> Some x.status in
             match x with
             | `Status status :: `Streams_event streams :: events ->
                let status = { status with streams } in
                ({ status; prev_status; events } : group) :: gps
             | _ -> assert false) []
    in groups, { acc with events = rest}

  let get_req_stack ({ status; _ } : group) (prev_t : group option)
      : probe_response probe_request list =
    let open Board_serializer in
    let bitrate =
      if not status.basic.has_sync then None else
        Some (Get_bitrates (Board_serializer.get_request_id ())) in
    let jitter = match status.jitter_mode with
      | None -> None
      | Some m ->
         (* request for jitter only if required stream is present *)
         let eq = Stream.Multi_TS_ID.equal in
         if not @@ List.mem ~eq m.stream status.streams then None else
           Some (Get_jitter { request_id = get_request_id ()
                            ; pointer = !jitter_ptr }) in
    let errors =
      if not status.errors then None else
        Some (Get_board_errors (get_request_id ())) in
    let ts_structs =
      let req =
        Get_ts_struct { request_id = get_request_id ()
                      ; stream = `All } in
      match prev_t with
      | None -> Some req
      | Some (old : group) ->
         let old = old.status.versions.ts_ver_com in
         let cur = status.versions.ts_ver_com in
         if old = cur then None else Some req in
    let t2mi_structs =
      let make_req stream_id =
        Get_t2mi_info { request_id = get_request_id ()
                      ; stream = status.t2mi_mode.stream
                      ; stream_id } in
      match status.t2mi_sync, prev_t with
      | [], _ -> []
      | sync, None -> List.map make_req sync
      | sync, Some prev ->
         (* XXX maybe we should request only those structures that really changed *)
         let old = prev.status.versions.t2mi_ver_lst in
         let cur = status.versions.t2mi_ver_lst in
         let eq = Equal.list (=) in
         begin match eq old cur with
         | true -> []
         | false -> List.map make_req sync
         end in
    List.(t2mi_structs
          |> cons_maybe ts_structs
          |> cons_maybe bitrate
          |> cons_maybe jitter
          |> cons_maybe errors)

  let to_ts_errors (g : group) =
    List.filter_map (function `Ts_errors x -> Some x | _ -> None) g.events

  let to_t2mi_errors (g : group) =
    List.filter_map (function `T2mi_errors x -> Some x | _ -> None) g.events

  let merge_streams streams l =
    List.filter_map (fun (id, x) ->
        match Stream.find_by_multi_id id streams with
        | None -> Logs.err (fun m -> m "Not found a stream for data!");
                  None
        | Some s -> Some (s.id, x)) l

  let to_raw_stream ({ input; t2mi } : init)
        (mode : t2mi_mode_raw)
        (i : input)
        (id : Stream.Multi_TS_ID.t) =
    let open Stream.Source in
    let open Stream.Raw in
    let source_id = Stream.Multi_TS_ID.source_id id in
    let stream_id = Stream.Multi_TS_ID.stream_id id in
    let spi_id = input_to_int SPI in
    let asi_id = input_to_int ASI in
    let src = match source_id, stream_id with
      | src, id when src = input && id = spi_id -> `Spi
      | src, id when src = input && id = asi_id -> `Asi
      | src, id when src = t2mi -> `T2mi id
      | _ -> begin match i with SPI -> `Spi | ASI -> `Asi end in
    let source = match src, mode with
      | `T2mi plp, { stream
                   ; t2mi_stream_id = stream_id
                   ; enabled = true
                   ; _ } ->
         let node = Stream (TS_multi stream) in
         let info = T2MI { stream_id; plp } in
         Some { node; info }
      | `T2mi _, _ -> None
      | `Spi, _ -> Some { node = Port spi_id; info = SPI }
      | `Asi, _ -> Some { node = Port asi_id; info = ASI }
      | `Unknown, _ -> None in
    match source with
    | None -> None
    | Some source ->
       let (typ : Stream.stream_type) =
         if Stream.Multi_TS_ID.equal id mode.stream && mode.enabled
         then T2MI else TS in
       Some { id = TS_multi id; source; typ }

  let handle_immediate (pe : push_events) (acc : Acc.t) : Acc.t =
    match acc.group with
    | None -> acc
    | Some ({ status = { basic; t2mi_mode; input; _ }; _ } : group) ->
       pe.input input;
       pe.status basic;
       pe.t2mi_mode_raw t2mi_mode;
       acc

  let merge_with_pid ~(eq : 'a -> 'a -> bool)
        (pids : ('a * (Pid.t list)) list)
        (errors : ('a * (Error.t list)) list)
      : ('a * (Error.t_ext list)) list =
    List.map (fun (id, errors) ->
        let pids = List.Assoc.get ~eq id pids in
        let errors =
          List.map (fun (e : Error.t) ->
              let pid =
                Option.get_or ~default:(e.pid, None)
                @@ List.find_map (fun ((id, info) : Pid.t) ->
                       if e.pid = id then Some (id, Some info) else None)
                @@ Option.get_or ~default:[] pids in
              Error.{ e with pid }) errors in
        id, errors) errors

  let handle (pe : push_events)
        (events : events)
        (sources : init)
        (acc : Acc.t) =
    let (({ status = { basic; t2mi_mode; input; streams = ids; _ }
          ; _ } : group) as group) =
      Option.get_exn acc.group in
    let time = basic.time in
    let timestamp = basic.time in
    let streams = React.S.value events.streams in
    (* Push raw streams *)
    pe.raw_streams
    @@ List.filter_map (to_raw_stream sources t2mi_mode input) ids;
    (* Push board errors *)
    begin match acc.probes.board_errors with
    | None -> ()
    | Some x ->
       Logs.warn (fun m ->
           m "got board errors: [%s]"
           @@ String.concat "; "
           @@ List.map (fun ({ code; count; _ } : Board_error.t) ->
                  Printf.sprintf "%d: %d" code count) x);
       pe.board_errors x
    end;
    (* Push jitter *)
    begin match acc.probes.jitter with
    | None -> ()
    | Some x ->
       jitter_ptr := x.next_ptr;
       pe.jitter x.measures
    end;
    (* Stream dependent events *)
    (* Push TS bitrates *)
    begin match acc.probes.bitrate with
    | None -> ()
    | Some x ->
       let v = merge_streams streams x in
       List.map (Pair.map2 (Time.stamp timestamp)) v
       |> pe.bitrates
    end;
    (* Push TS structures *)
    begin match acc.probes.ts_structs with
    | None -> ()
    | Some x ->
       let structures = List.map (Pair.map2 (fun x -> { x with time })) x in
       pe.structures structures;
    end;
    (* Push TS errors *)
    begin match to_ts_errors group with
    | [] -> ()
    | v ->
       let open Error in
       let pids =
         List.map (fun (id, (x : structure)) -> id, x.pids)
         @@ React.S.value events.raw.structures in
       merge_with_pid ~eq:Stream.Multi_TS_ID.equal pids v
       |> merge_streams streams
       |> List.map (Pair.map2 (List.map (fun (x : 'a error) -> { x with time })))
       |> List.sort (fun (_, a) (_, b) ->
              Ord.list (fun a b -> Ptime.compare a.time b.time) a b)
       |> pe.ts_errors
    end;
    (* Push T2-MI info *)
    (* FIXME do smth if corresponding stream is not found *)
    begin match acc.probes.t2mi_info with
    | None -> ()
    | Some x ->
       merge_streams streams x
       |> List.map (Pair.map2 (Time.stamp timestamp))
       |> pe.t2mi_info
    end;
    (* Push T2-MI errors *)
    begin match to_t2mi_errors group with
    | [] -> ()
    | v ->
       let open Error in
       merge_streams streams v
       |> List.map (Pair.map2 (List.map (fun x -> { x with time })))
       |> pe.t2mi_errors
    end;
    { acc with probes = Acc.probes_empty }

end
