open Containers
open Board_types
open Board_api_common
open Api.Interaction
open Api.Interaction.Json
open Common
open Types

module WS = struct

  open Board_types.Streams.T2MI

  let to_yojson f v = Json.(List.to_yojson (Pair.to_yojson Int.to_yojson f) v)

  let structure (events:events) id _ body sock_data () =
    let e = React.E.fmap (List.Assoc.get ~eq:(=) id) events.streams.t2mi_structures in
    Api.Socket.handler socket_table sock_data e structure_to_yojson body

  let structures (events:events) ids _ body sock_data () =
    let e   = match ids with
      | [] -> events.streams.t2mi_structures
      | l  -> React.E.fmap (fun structures ->
                  List.filter (fun (id,_) -> List.mem ~eq:(=) id l) structures
                  |> function [] -> None | l -> Some l)
                events.streams.t2mi_structures
    in Api.Socket.handler socket_table sock_data e (to_yojson structure_to_yojson) body


  open Errors

  let rec filter (acc:Stream.id * t list) = function
    | []    -> true
    | f::tl -> (match f acc with
                | _,[] -> false
                | acc  -> filter acc tl)

  let to_yojson = Json.(Pair.to_yojson Stream.id_to_yojson (List.to_yojson to_yojson))

  let flst fltr f = match fltr with
    | [] -> None
    | l  -> Some (f l)

  let errors (events:events) ids t2mi_ids errors pids _ body sock_data () =
    let ids = List.map Stream.id_of_int32 ids in
    let f_stream = flst ids      (fun l (s,e) -> if List.mem ~eq:Stream.equal_id s l then s,e else s,[]) in
    let f_t2mi   = flst t2mi_ids (fun l (s,e) -> s,List.filter (fun x -> let id = Int32.to_int x.param_2 in
                                                                         List.mem ~eq:(=) id l) e) in
    let f_errors = flst errors   (fun l (s,e) -> s,List.filter (fun x -> List.mem ~eq:(=) x.err_code l) e) in
    let f_pids   = flst pids     (fun l (s,e) -> s,List.filter (fun x -> List.mem ~eq:(=) x.pid l) e) in
    let fns = List.filter_map (fun x -> x) [f_stream;f_t2mi;f_errors;f_pids] in
    let e   = React.E.fmap (fun (l:errors) ->
                  match List.filter (fun errs -> filter errs fns) l with
                  | [] -> None
                  | l  -> Some l) events.errors.t2mi_errors
    in Api.Socket.handler socket_table sock_data e (Json.List.to_yojson to_yojson) body

end

module HTTP = struct

  open Board_types.Streams.T2MI

  let sequence (api:api) id (duration:Time.Relative.t option) _ _ () =
    let seconds = Option.flat_map Time.Relative.to_int_s duration in
    api.get_t2mi_seq seconds
    >|= (fun x -> List.filter (fun (x:sequence_item) -> id = x.stream_id) x
                  |> sequence_to_yojson
                  |> Result.return)
    >>= respond_result

  let structure (api:api) id _ _ () =
    match List.Assoc.get ~eq:(=) id @@ api.get_t2mi_structures () with
    | None   -> `String "stream not found" |> Result.fail |> respond_result ~err_status:`Not_found
    | Some s -> structure_to_yojson s |> Result.return |> respond_result

  let to_yojson f v = Json.(List.to_yojson (Pair.to_yojson Int.to_yojson f) v)

  let structures (api:api) ids _ _ () =
    let structs = match ids with
      | [] -> api.get_t2mi_structures ()
      | l  -> List.filter (fun (id,_) -> List.mem ~eq:(=) id l)
                (api.get_t2mi_structures ())
    in (to_yojson structure_to_yojson) structs |> Result.return |> respond_result

  module Archive = struct

    type struct_t2 = (int * Board_types.Streams.T2MI.structure * Time.t) list [@@deriving yojson]

    let structure db id limit from till duration _ _ () =
      match Time.make_interval ?from ?till ?duration () with
      | Ok `Range (from,till) ->
         Db.Streams.select_structs_t2 db ~with_pre:true ?limit ~ids:[id] ~from ~till
         |> Lwt_result.map (fun d -> Api.Api_types.rows_to_yojson struct_t2_to_yojson (fun () -> `Null) d)
         |> Lwt_result.map_err (fun s -> (`String s : Yojson.Safe.json))
         >>= fun x -> respond_result x
      | _ -> respond_error ~status:`Not_implemented "FIXME" ()

    let structures db ids limit from till duration _ _ () =
      match Time.make_interval ?from ?till ?duration () with
      | Ok `Range (from,till) ->
         Db.Streams.select_structs_t2 db ~with_pre:true ?limit ~ids ~from ~till
         |> Lwt_result.map (fun d -> Api.Api_types.rows_to_yojson struct_t2_to_yojson (fun () -> `Null) d)
         |> Lwt_result.map_err (fun s -> (`String s : Yojson.Safe.json))
         >>= fun x -> respond_result x
      | _ -> respond_error ~status:`Not_implemented "FIXME" ()

  end

end

let handler db (api:api) events =
  let open Uri in
  let open Boards.Board.Api_handler in
  create_dispatcher
    "t2mi"
    [ create_ws_handler ~docstring:"Pushes stream structure to the client"
        ~path:Path.Format.("structures" @/ Int ^/ empty)
        ~query:Query.empty
        (WS.structure events)
    ]
    [ `GET, [ create_handler ~docstring:"Returns T2-MI stream structure (L1 signalling)"
                ~path:Path.Format.("structures" @/ Int ^/ empty)
                ~query:Query.empty
                (HTTP.structure api)
            ; create_handler ~docstring:"Returns T2-MI packet sequence"
                ~path:Path.Format.("packets" @/ Int ^/ empty)
                ~query:Query.[ "duration", (module Option(Time.Relative)) ]
                (HTTP.sequence api)
            (* Archive *)
            ; create_handler ~docstring:"Returns archived stream structure"
                ~path:Path.Format.("archive/structures" @/ Int ^/ empty)
                ~query:Query.[ "limit",    (module Option(Int))
                             ; "from",     (module Option(Time.Show))
                             ; "to",       (module Option(Time.Show))
                             ; "duration", (module Option(Time.Relative)) ]
                (HTTP.Archive.structure db)
            ]
    ]
