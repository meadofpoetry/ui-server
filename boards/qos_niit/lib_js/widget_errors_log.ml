open Containers
open Components
open Common
open Board_types
open Board_types.Streams.TS
open Lwt_result.Infix
open Api_js.Api_types

type config =
  { stream : Stream.t
  }

let name = "Errors"

let base_class = "qos-niit-errors-log"

let settings = None

let make_table is_hex (init:Errors.raw) =
  let hex_id_fmt = Some (Printf.sprintf "0x%04X") in
  let tz_offset_s = Ptime_clock.current_tz_offset_s () in
  let show_time = Time.to_human_string ?tz_offset_s in
  let fmt =
    let open Table in
    let open Format in
    (to_column ~sortable:true "Время", Time (Some show_time))
    :: (to_column ~sortable:true "Сервис",       Option (String None, ""))
    :: (to_column ~sortable:true "Число ошибок", Int None)
    :: (to_column ~sortable:true "PID",          Int None)
    :: (to_column ~sortable:true "Событие",      String None)
    :: (to_column "Подробности",                 String None)
    :: [] in
  let table = new Table.t ~dense:true ~fmt () in
  let on_change = fun (x:bool) ->
    List.iter (fun row ->
        let open Table in
        match row#cells with
        | _ :: _ :: _ :: pid :: _ ->
           let fmt  = if x then Int hex_id_fmt else Int None in
           pid#set_format fmt)
      table#rows in
  if is_hex then on_change true;
  table, on_change

let make_card (init:Errors.raw) =
  (* FIXME should remember preffered state *)
  let is_hex = false in
  let table, on_change = make_table is_hex init in
  let actions = new Card.Actions.t ~widgets:[] () in
  let media = new Card.Media.t ~widgets:[ table ] () in
  let card =
    new Card.t ~widgets:[ actions#widget
                        ; (new Divider.t ())#widget
                        ; media#widget ] () in
  let add_row (id, (error:Errors.t)) =
    let service = None in
    let date = error.timestamp in
    let pid = error.pid in
    let count = error.count in
    let check =
      let num, name = Ts_error.to_name error in
      num ^ " " ^ name in
    let extra = Ts_error.Description.of_ts_error error in
    table#add_row (date :: service :: count :: pid :: check :: extra :: []) in
  let switch = new Switch.t ~state:is_hex ~on_change () in
  let hex = new Form_field.t ~input:switch ~label:"HEX IDs" () in
  actions#append_child hex;
  List.iter Fun.(ignore % add_row) @@ List.rev init;
  let () = card#add_class base_class in
  card

let make init = make_card init
