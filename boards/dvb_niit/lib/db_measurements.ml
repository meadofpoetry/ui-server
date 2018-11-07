open Containers
open Storage.Database
open Board_types
open Db_common
open Printf
open Api.Api_types
open Common

let typ =
  Types.custom
    Types.(List.(int & ID.db & bool
                 & option float & option float & option float
                 & option int & option int & ptime))
    ~encode:(fun (id, ({ data; timestamp } : Measure.t Time.timestamped)) ->
      Ok (0,
          (ID.to_db id,
           (data.lock,
            (data.power,
             (data.mer,
              (data.ber,
               (data.freq,
                (data.bitrate, timestamp)))))))))
    ~decode:(fun (_,
                  (id,
                   (lock,
                    (power,
                     (mer,
                      (ber,
                       (freq,
                        (bitrate, timestamp)))))))) ->
      let (data : Measure.t) = { lock; power; mer; ber; freq; bitrate } in
      Ok (ID.of_db id, { data; timestamp }))

let insert db meas =
  let table = (Conn.names db).measurements in
  let insert =
    R.exec typ
      (sprintf {|INSERT INTO %s(tuner,stream,lock,power,mer,ber,freq,bitrate,date)
                VALUES (?,?,?,?,?,?,?,?,?)|} table)
  in
  Conn.request db Request.(exec insert meas)

let select db ?(streams = []) ?(tuners = []) ?(limit = 500) ?(order = `Desc)
      ~from ~till () =
  let table = (Conn.names db).measurements in
  let streams = is_in "stream" ID.to_value_string streams in
  let tuners = is_in "tuner" string_of_int tuners in
  let ord = match order with `Desc -> "DESC" | `Asc -> "ASC" in
  let select =
    R.collect Types.(tup3 ptime ptime int) typ
      (sprintf {|SELECT tuner,stream,lock,power,mer,ber,freq,bitrate,date FROM %s
                WHERE %s %s date >= $1 AND date <= $2
                ORDER BY date %s
                LIMIT $3|}
         table streams tuners ord)
  in
  Conn.request db Request.(
    list select (from, till, limit)
    >>= fun data ->
    let rec aux acc = function
      | [] -> acc
      | (id, m) :: tl ->
         let acc =
           List.Assoc.update ~eq:Stream.ID.equal
             ~f:(function None -> Some [m] | Some l -> Some (m :: l))
             id acc in
         aux acc tl in
    return (Raw { data = aux [] data
                ; has_more = List.length data >= limit
                ; order }))
