open Containers
open Storage.Database
open Lwt.Infix

let merge slst =
  let open Common.Stream in
  let uniq = List.uniq ~eq:(fun s1 s2 -> Equal.poly s1.id s2.id) in
  React.S.merge ~eq:Equal.physical (fun acc l -> l @ acc) [] slst
  |> React.S.map uniq
   
let stream = Caqti_type.custom
               Caqti_type.(let (&) = tup2 in
                           int32 & string & string & int32 & option string)
               ~encode:(fun (id, inp, inp_name, par_id, desc) ->
                 Ok(id,(inp,(inp_name,(par_id,(desc))))))
               ~decode:(fun (id,(inp,(inp_name,(par_id,(desc))))) ->
                 Ok(id, inp, inp_name, par_id, desc))
   
let init o =
  let create =
    Caqti_request.exec Caqti_type.unit
      {eos|CREATE TABLE IF NOT EXISTS streams(
       stream_id   INTEGER,
       input       TEXT,
       input_name  TEXT,
       parent_id   INTEGER,
       description TEXT
       )|eos}
  in
  let init' (module Db : Caqti_lwt.CONNECTION) =
    Db.exec create () >>= function
    | Ok v    -> Lwt.return v
    | Error e -> Lwt.fail_with (Caqti_error.show e)
  in Storage.Database.with_connection o init'

let store_streams dbs streams =
  let open Common.Stream in
  let rec get_input s =
    match s.source with
    | Input i -> i
    | Parent p -> get_input p
  in
  let get_id s =
    match s.id with
    | `Ts id -> id_to_int32 id
    | `Ip ad -> Ipaddr.V4.to_int32 ad.ip
  in
  let form_req stream =
    let stream_id = get_id stream in
    let input, input_name =
      let i = get_input stream in
      let n = Common.Topology.input_to_string i.input in
      n, (Printf.sprintf "%s-%d" n i.id)
    in
    let parent_id = match stream.source with
      | Parent p -> get_id p
      | Input _  -> 0l
    in
    (stream_id, input, input_name, parent_id, stream.description)
  in
  let insert =
    Caqti_request.exec stream
      {|INSERT INTO streams(stream_id,input,input_name,parent_id,description)
       VALUES (?,?,?,?,?)|}
  in
  let delete =
    Caqti_request.exec Caqti_type.unit
      {|DELETE FROM streams|}
  in
  let store_streams' (module Db : Caqti_lwt.CONNECTION) =
    List.fold_left (fun acc s -> acc >>= function
                                 | Ok () -> Db.exec insert (form_req s)
                                 | e -> Lwt.return e)
      (Db.exec delete ()) streams
    >>= function
    | Ok v    -> Lwt.return v
    | Error _ -> Lwt.fail_with "store_streams"
  in Storage.Database.with_connection dbs store_streams'

type _ req =
  | Store_streams : Common.Stream.t list -> unit Lwt.t req

let request (type a) o (req : a req) : a =
  match req with
  | Store_streams s -> store_streams o s
