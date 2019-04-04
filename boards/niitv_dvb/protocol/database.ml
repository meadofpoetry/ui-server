open Application_types
open Board_niitv_dvb_types
open Api

module R = Caqti_request

(* module SID = struct
 * 
 *   type t = Stream.ID.t
 *   let db_type : string = "UUID"
 *   let of_stream_id (id : Stream.ID.t) =
 *     Stream.ID.to_string id
 *   let to_stream_id (t : string) =
 *     Stream.ID.of_string t
 *   let to_value_string x =
 *     let s = of_stream_id x in
 *     Printf.sprintf "'%s'::%s" s db_type
 *   let typ =
 *     Caqti_type.custom
 *       ~encode:(fun x -> Ok (of_stream_id x))
 *       ~decode:(fun x -> try Ok (to_stream_id x) with _ -> Error "Bad SID")
 *       Caqti_type.string
 * end *)

module Model = struct
  open Db.Key

  type init = int

  type names =
    { measurements : string
    }

  let name = "dvb_niit"

  let keys_measurements =
    Db.make_keys ~time_key:"date"
      [ "tuner", key ~primary:true "INTEGER"
      ; "lock", key "BOOL"
      ; "power", key "REAL"
      ; "mer", key "REAL"
      ; "ber", key "REAL"
      ; "freq", key "INTEGER"
      ; "bitrate", key "INTEGER"
      ; "date", key ~primary:true "TIMESTAMP"
      ]

  let tables id =
    let id = string_of_int id in
    let names =
      { measurements = "dvb_niit_measurements_" ^ id
      } in
    names,
    [names.measurements, keys_measurements, None]
end

module Conn = Db.Make(Model)

let is_in field to_string = function
  | [] -> ""
  | lst -> Printf.sprintf " %s IN (%s) AND "
             field (String.concat "," @@ List.map to_string lst)

module Measurements = struct

  let typ =
    Caqti_type.custom
      Caqti_type.(let ( & ) = tup2 in
                  List.(int & bool & option float & option float & option float
                        & option int & option int & ptime))
      ~encode:(fun (id, ({ data = { lock; power; mer; ber; freq; bitrate }
                         ; timestamp } : Measure.t ts)) ->
        Ok (id, (lock, (power, (mer, (ber, (freq, (bitrate, timestamp))))))))
      ~decode:(fun (tuner, (lock, (power, (mer, (ber, (freq, (bitrate, timestamp))))))) ->
        let (data : Measure.t) = { lock; power; mer; ber; freq; bitrate } in
        Ok (tuner, { data; timestamp }))

  let insert db meas =
    let open Printf in
    let table = (Conn.names db).measurements in
    let insert =
      R.exec typ
        (sprintf {|INSERT INTO %s(tuner,stream,lock,power,mer,ber,freq,bitrate,date)
                  VALUES (?,?,?,?,?,?,?,?,?)|} table) in
    Conn.request db Db.Request.(
      with_trans (List.fold_left (fun acc x ->
                      acc >>= fun () -> exec insert x)
                    (return ()) meas))

  let select db ?(streams = []) ?(tuners = []) ?(limit = 500) ?(order = `Desc)
        ~from ~till () =
    let open Printf in
    let table = (Conn.names db).measurements in
    (* let streams = is_in "stream" SID.to_value_string streams in *)
    let tuners = is_in "tuner" string_of_int tuners in
    let ord = match order with `Desc -> "DESC" | `Asc -> "ASC" in
    let select =
      R.collect Caqti_type.(tup3 ptime ptime int) typ
        (sprintf {|SELECT tuner,lock,power,mer,ber,freq,bitrate,date FROM %s
                  WHERE %s date >= $1 AND date <= $2
                  ORDER BY date %s
                  LIMIT $3|}
           table (* streams *) tuners ord) in
    Conn.request db Db.Request.(
      list select (from, till, limit)
      >>= fun data ->
      try
        let rec aux acc = function
          | [] -> acc
          | (id, m) :: tl ->
             let acc =
               Boards.Util.List.Assoc.update ~eq:(=)
                 (function None -> Some [m] | Some l -> Some (m :: l))
                 id acc in
             aux acc tl in
        return (Ok (Raw { data = aux [] data
                        ; has_more = List.length data >= limit
                        ; order }))
      with Failure e -> return (Error e))
end
