open Containers
open Storage.Database
open Common.Stream
open Api.Api_types
open Lwt.Infix

module SID = struct

  type t = ID.t
  let db_type : string = "UUID"
  let of_stream_id (id : ID.t) =
    ID.to_string id
  let to_stream_id (t : string) =
    ID.of_string t
  let to_value_string x =
    let s = ID.to_string x in
    Printf.sprintf "'%s'::%s" s db_type
  let typ = Caqti_type.custom
              ~encode:(fun x -> Ok (of_stream_id x))
              ~decode:(fun x -> try Ok (to_stream_id x) with _ -> Error "Bad SID")
              Caqti_type.string
end

let log_entry =
  let open Common.Topology in
  let open Log_message in
  Caqti_type.custom
    Caqti_type.(let (&) = tup2 in
                ptime & int & string & string &
                  (option int) & (option string) &
                    (option int) & (option int) &
                      (option SID.typ) & (option int) &
                        (option string) & (option string))
    ~encode:(fun (l : t) ->
      let level = level_to_enum l.level in
      let input_id = Option.map (fun (i : topo_input) -> i.id) l.input in
      let input_typ = Option.map (fun (i : topo_input) -> input_to_enum i.input) l.input in
      let board = match l.node with Some (Board x) -> Some x | _ -> None in
      let cpu = match l.node with Some (Cpu x) -> Some x | _ -> None in
      let pid = Option.map (fun (x : pid) -> x.id) l.pid in
      let pid_typ = Option.flat_map (fun (x : pid) -> x.typ) l.pid in
      Ok(l.time,
         (level,
          (l.message,
           (l.info,
            (board,
             (cpu,
              (input_id,
               (input_typ,
                (l.stream,
                 (pid,
                  (pid_typ,
                   (l.service)))))))))))))
    ~decode:(fun (time,
                  (level,
                   (message,
                    (info,
                     (board,
                      (cpu,
                       (input_id,
                        (input_typ,
                         (stream,
                          (pid,
                           (pid_typ,
                            (service)))))))))))) ->
      let open Common.Topology in
      let level = Option.get_exn (Log_message.level_of_enum level) in
      let pid = match pid with
        | None -> None
        | Some id -> Some { id; typ = pid_typ } in
      let node = match board, cpu with
        | None, None -> None
        | Some x, None -> Some (Log_message.Board x)
        | None, Some x -> Some (Cpu x)
        | Some _, Some _ -> assert false in
      let input = Option.map2 (fun input id -> { input; id })
                    Option.(input_typ >>= input_of_enum)
                    input_id in
      Ok { time; level; node; input; message; info; stream; pid; service })

module Model = struct
  open Key_t

  type init = ()

  type names = { log : string }

  let name = "application"

  let log_keys = make_keys ~time_key:"date"
                   [ "date", key "TIMESTAMP"
                   ; "level", key "INTEGER"
                   ; "message", key "VARCHAR(80)"
                   ; "info", key "VARCHAR(400)"
                   ; "board", key "INTEGER"
                   ; "cpu", key "VARCHAR(20)"
                   ; "input_id", key "INTEGER"
                   ; "input_type", key "INTEGER"
                   ; "stream", key SID.db_type
                   ; "pid", key "INTEGER"
                   ; "pid_type", key "VARCHAR(256)" (* FIXME length *)
                   ; "service", key "VARCHAR(256)" (* TODO define length *)
                   ]

  let tables () =
    let names = { log = "log" } in
    names,
    [ names.log, log_keys, None ]

end

module Conn = Storage.Database.Make(Model)

module R = Caqti_request

let is_in field to_string = function
  | [] -> ""
  | lst -> Printf.sprintf " %s IN (%s) AND "
             field (String.concat "," @@ List.map to_string lst)

let is_null_or_in field to_string =
  function
  | [] -> ""
  | lst -> Printf.sprintf " (%s IS null OR %s IN (%s)) AND "
             field field (String.concat "," @@ List.map to_string lst)

module Log = struct

  let insert db entries =
    let open Printf in
    let table = (Conn.names db).log in
    let insert_row =
      R.exec log_entry
        (sprintf "INSERT INTO %s
                  (date,level,message,info,board,cpu,
                  input_id,input_type,stream,pid,
                  pid_type,service)
                  VALUES (?,?,?,?,?,?,?,?,?,?,?,?)" table)
    in Conn.request db Request.(
      with_trans (entries
                  |> List.fold_left (fun acc entry ->
                         acc >>= fun () ->
                         exec insert_row entry)
                       (return ())))

  let max_limit = 500

  let select db ?(boards = []) ?(cpu = []) ?(inputs = []) ?(streams = [])
        ?limit ~from ~till () =
    let open Printf in
    let open Common.Topology in
    let table = (Conn.names db).log in
    let boards = is_null_or_in "board" string_of_int boards in
    let cpu = is_null_or_in "cpu" (fun x -> Printf.sprintf "'%s'" x) cpu in
    let streams = is_null_or_in "stream" SID.to_value_string streams in
    let inputs = 
      let to_string (i : topo_input) =
        Printf.sprintf "(%d, %d)" i.id (input_to_enum i.input) in
      is_null_or_in "(input_id, input_type)"
        to_string inputs in
    let limit = match limit with
      | None -> max_limit
      | Some l when l > max_limit -> max_limit
      | Some l -> l in
    let select' =
      R.collect Caqti_type.(tup3 ptime ptime int) log_entry
        (sprintf {|SELECT date,level,message,info,board,cpu,
                  input_id,input_type,stream,pid,pid_type,service
                  FROM %s WHERE %s %s %s %s date >= $1 AND date <= $2
                  ORDER BY date DESC LIMIT $3|}
           table boards cpu inputs streams)
    in
    Conn.request db Request.(
      list select' (from, till, limit)
      >>= fun data ->
      return (Raw { data
                  ; has_more = (List.length data >= limit)
                  ; order = `Desc }))
end
