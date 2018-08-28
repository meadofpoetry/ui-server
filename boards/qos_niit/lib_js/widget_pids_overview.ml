open Containers
open Components
open Common
open Board_types.Streams.TS
open Lwt_result.Infix
open Api_js.Api_types

type config =
  { stream : Stream.t
  } [@@deriving yojson]

let name = "PIDs"

let base_class = "qos-niit-pid-overview"

let settings = None

let get_pids ~id control =
  Requests.Streams.HTTP.get_pids ~id ~limit:1 control
  >>= (function
       | Raw s ->
          (match List.head_opt s.data with
           | Some (_, { timestamp; pids }) -> Some timestamp, pids
           | None -> None, [])
          |> Lwt_result.return
       | _     -> Lwt.fail_with "got compressed")
  |> Lwt_result.map_err Api_js.Requests.err_to_string

let to_pid_extra (has_pcr:bool) (is_scrambled:bool) =
  let pcr = match has_pcr with
    | false -> None
    | true  ->
       Some Icon.SVG.(new t ~paths:Path.[ new t clock_outline () ] ()) in
  let scr = match is_scrambled with
    | false -> None
    | true  ->
       Some Icon.SVG.(new t ~paths:Path.[ new t lock () ] ()) in
  let widgets = List.(cons_maybe pcr (cons_maybe scr [])) in
  new Hbox.t ~widgets ()

let update_row row total br pid =
  let cur, per, min, max =
    let open Table in
    match row#cells with
    | _ :: _ :: _ :: _ :: a :: b :: c :: d :: _ ->
       a, b, c, d in
  let pct = 100. *. (float_of_int br)
            /. (float_of_int total) in
  let br = (float_of_int br) /. 1_000_000. in
  cur#set_value @@ Some br;
  per#set_value @@ Some pct;
  (match min#value with
   | None -> min#set_value (Some br)
   | Some v -> if br <. v then min#set_value (Some br));
  (match max#value with
   | None -> max#set_value (Some br)
   | Some v -> if br >. v then max#set_value (Some br));
  br, pct

let make_table (is_hex : bool)
      (init : pid_info list) =
  let open Table in
  let dec_pid_fmt = Int None in
  let hex_pid_fmt = Int (Some (Printf.sprintf "0x%04X")) in
  let br_fmt = Option (Float None, "-") in
  let pct_fmt = Option (Float (Some (Printf.sprintf "%.2f")), "-") in
  let fmt =
    let open Format in
    let to_sort_column = to_column ~sortable:true in
    (to_sort_column "PID", dec_pid_fmt)
    :: (to_sort_column "Тип", String None)
    :: (to_column "Доп. инфо", Widget None)
    :: (to_sort_column "Сервис", Option (String None, ""))
    :: (to_sort_column "Битрейт, Мбит/с", br_fmt)
    :: (to_sort_column "%", pct_fmt)
    :: (to_sort_column "Min, Мбит/с", br_fmt)
    :: (to_sort_column "Max, Мбит/с", br_fmt)
    :: [] in
  let table = new t ~dense:true ~fmt () in
  let on_change = fun (x : bool) ->
    List.iter (fun row ->
        let open Table in
        match row#cells with
        | pid :: _ ->
           pid#set_format (if x then hex_pid_fmt else dec_pid_fmt))
      table#rows in
  if is_hex then on_change true;
  table, on_change

let add_row (table : 'a Table.t) (pid : pid_info) =
  let open Table in
  let pid_type = match pid.pid_type with
    | SEC l ->
       let s = List.map Fun.(Mpeg_ts.(table_to_string % table_of_int)) l
               |> String.concat ", " in
       "SEC -> " ^ s
    | PES x ->
       let s = Mpeg_ts.stream_type_to_string x in
       "PES -> " ^ s
    | ECM x -> "ECM -> " ^ (string_of_int x)
    | EMM x -> "EMM -> " ^ (string_of_int x)
    | Null -> "Null"
    | Private -> "Private" in
  let extra = to_pid_extra pid.has_pcr pid.scrambled in
  let data = Data.(
      pid.pid :: pid_type :: extra :: pid.service
      :: None :: None :: None :: None :: []) in
  let row = table#add_row data in
  row

let make_timestamp_string (timestamp : Time.t option) =
  let tz_offset_s = Ptime_clock.current_tz_offset_s () in
  let s = match timestamp with
    | None -> "-"
    | Some t -> Time.to_human_string ?tz_offset_s t
  in
  "Обновлено: " ^ s

class t (timestamp : Time.t option)
        (init : pid_info list)
        (control : int)
        () =
  (* FIXME should remember preffered state *)
  let is_hex = false in
  let table, on_change = make_table is_hex init in
  let title = "Обзор" in
  let subtitle = make_timestamp_string timestamp in
  let switch = new Switch.t ~state:is_hex ~on_change () in
  let hex = new Form_field.t ~input:switch ~label:"HEX IDs" () in
  let media = new Card.Media.t ~widgets:[ table ] () in
  let set_rate = function
    | None -> () (* FIXME do smth *)
    | Some (total, (rate:(int * int) list)) ->
       List.fold_left (fun rows (pid, br) ->
           let open Table in
           match List.find_opt (fun (row : 'a Row.t) ->
                     let cell = match row#cells with a :: _ -> a in
                     cell#value = pid) rows with
           | Some x ->
              update_row x total br pid |> ignore;
              List.remove ~eq:Equal.physical ~x rows
           | None   -> rows) table#rows rate
       |> ignore in
  let title' = new Card.Primary.title title () in
  let subtitle' = new Card.Primary.subtitle subtitle () in
  let text_box = Widget.create_div () in
  let primary = new Card.Primary.t ~widgets:[text_box] () in
  object(self)
    inherit Card.t ~widgets:[ ] ()

    (** Updates bitrate values *)
    method set_rate = set_rate

    method table = table

    method switch = hex

    method set_hex x = on_change x

    initializer
      List.iter Fun.(ignore % add_row table) init;
      text_box#append_child title';
      text_box#append_child subtitle';
      self#add_class base_class;
      self#append_child primary#widget;
      self#append_child @@ new Divider.t ();
      self#append_child media#widget;
  end

let make ?(init : pids option)
      (stream : Stream.t)
      control =
  let init = match init with
    | Some x -> Lwt_result.return (Some x.timestamp, x.pids)
    | None -> get_pids ~id:stream.id control in
  init
  >|= (fun (ts, data) -> new t ts data control ())
  |> Ui_templates.Loader.create_widget_loader

