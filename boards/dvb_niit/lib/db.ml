open Containers
open Storage.Database
open Board_types
open Lwt.Infix
   
(* type _ req =
 *   | Store_measures : Board_types.measures -> unit req
 * 
 * let name = "dvb_niit"
 * 
 * let table = "dvb_meas"
 *   
 * let measure = Caqti_type.custom
 *                 Caqti_type.(let (&) = tup2 in
 *                             int & bool & (option float) & (option float) & (option float)
 *                             & (option int) & (option int) & ptime)
 *                 ~encode:(fun (m:measures) ->
 *                   Ok(m.id, (m.lock,(m.power,(m.mer,(m.ber,(m.freq,(m.bitrate,m.timestamp))))))))
 *                 ~decode:(fun (id,(lock,(power,(mer,(ber,(freq,(bitrate,(timestamp)))))))) ->
 *                   Ok({ id; lock; power; mer; ber; freq; bitrate; timestamp }))
 *   
 * let init db =
 *   let create_video =
 *     Caqti_request.exec Caqti_type.unit
 *       {eos|
 *        CREATE TABLE IF NOT EXISTS dvb_meas(
 *        tun     INTEGER,
 *        lock    BOOL,
 *        power   REAL,
 *        mer     REAL,
 *        ber     REAL,
 *        freq    INTEGER,
 *        bitrate INTEGER,
 *        date    TIMESTAMP DEFAULT CURRENT_TIMESTAMP
 *        )
 *        |eos}
 *   in db.exec (Exec create_video) ()
 * 
 * let store_measures db m =
 *   let insert =
 *     Caqti_request.exec measure
 *       {|INSERT INTO dvb_meas(tun,lock,power,mer,ber,freq,bitrate,date)
 *        VALUES (?,?,?,?,?,?,?,'epoch'::TIMESTAMP + ? * '1 second'::INTERVAL)|}
 *   in db.exec (Exec insert) m
 * 
 * let request (type a) o (req : a req) : a Lwt.t =
 *   match req with
 *   | Store_measures m -> store_measures o m
 * 
 * let worker = None                *)
