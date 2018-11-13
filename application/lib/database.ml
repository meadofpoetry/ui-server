open Containers
open Storage.Database
open Common.Stream
open Api.Api_types
   
open Lwt.Infix

module SID = struct

  type t = ID.t
  let db_type : string = "UUID"
  let of_stream_id (id:ID.t) =
    ID.to_string id 
  let to_stream_id (t:string) =
    ID.of_string t
  let typ     = Caqti_type.custom
                  ~encode:(fun x -> Ok (of_stream_id x))
                  ~decode:(fun x -> try Ok (to_stream_id x) with _ -> Error "Bad SID")
                  Caqti_type.string
end

let log_entry =
  Caqti_type.custom
    Caqti_type.(let (&) = tup2 in
                ptime & int & string & string &
                  (option int) & (option int) &
                    (option SID.typ) & (option int) & (option string))
    ~encode:(fun (l : Log_message.t) ->
      let level = Log_message.level_to_enum l.level in
      let input_id = Option.map (fun i -> Common.Topology.(i.id)) l.input in
      let input_typ = Option.map (fun i -> Common.Topology.(input_to_enum i.input)) l.input in
      Ok(l.time,(level,(l.message,(l.info,(input_id,(input_typ,(l.stream,(l.pid,(l.service))))))))))
    ~decode:(fun (time,(level,(message,(info,(input_id,(input_typ,(stream,(pid,(service))))))))) ->
      let open Common.Topology in
      let level = Option.get_or ~default:Log_message.Low
                    (Log_message.level_of_enum level)
      in
      let input = Option.map2 (fun input id -> { input; id })
                    Option.(input_typ >>= input_of_enum)
                    input_id
      in
      Ok { time; level; input;
           message; info; stream;
           pid; service;} )

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
                   ; "input_id", key "INTEGER"
                   ; "input_type", key "INTEGER"
                   ; "stream", key "INTEGER"
                   ; "pid", key "INTEGER"
                   ; "service", key "SERVICE"
                   ]

  let tables () =
    let names = { log = "log" } in
    names,
    [ names.log, log_keys, None ]

end

module Conn = Storage.Database.Make(Model)

module R = Caqti_request

module Log = struct

  let insert db entries =
    let open Printf in
    let table = (Conn.names db).log in
    let insert_row = R.exec log_entry
                       (sprintf "INSERT INTO %s (date,level,message,info,input_id,input_type,stream,pid,service) VALUES (?,?,?,?,?,?,?,?,?)" table)
    in Conn.request db Request.(with_trans (entries
                                            |> List.fold_left (fun acc entry ->
                                                   acc >>= fun () ->
                                                   exec insert_row entry)
                                                 (return ())))

  let max_limit = 500
            
  let select db input ?id ?limit ~from ~till =
    let open Printf in
    let open Common.Topology in
    let table     = (Conn.names db).log in
    let input_id  = input.id in
    let input_typ = input_to_enum input.input in
    let limit     = match limit with
      | None -> max_limit
      | Some l when l > max_limit -> max_limit
      | Some l -> l
    in
    let select' = R.collect Caqti_type.(tup4 int int ptime (tup2 ptime int)) log_entry
                    (sprintf {|SELECT date,level,message,info,input_id,input_type,
                                      stream,pid,service
                              FROM %s WHERE input_id = $1 AND input_type = $2
                              AND date >= $3 AND date <= $4
                              ORDER BY date DESC LIMIT $5|} table)
    and select_ids' = R.collect Caqti_type.(tup4 int int SID.typ (tup3 ptime ptime int)) log_entry
                        (sprintf {|SELECT date,level,message,info,input_id,input_type,
                                          stream,pid,service
                                  FROM %s WHERE 
                                  input_id = $1 AND input_type = $2 AND stream = $3
                                  AND date >= $4 AND date <= $5
                                  ORDER BY date DESC LIMIT $6|} table)
    in Conn.request db Request.(begin match id with
                                | None -> list select' (input_id,input_typ,from,(till,limit))
                                | Some id -> list select_ids'(input_id,input_typ,id,(from,till,limit))
                                end >>= fun data ->
                                return (Raw { data
                                            ; has_more = (List.length data >= limit)
                                            ; order = `Desc }))
end
