open Containers
open Components
open Common
open Board_types.Streams.TS
open Lwt_result.Infix
open Api_js.Api_types

type config =
  { stream : Stream.t
  }

let ( % ) = Fun.( % )
let ( >>* ) x f = Lwt_result.map_err f x

let get_tables ~id control =
  Requests.Streams.HTTP.get_tables ~id ~limit:1 control
  >>* Api_js.Requests.err_to_string
  >>= function
  | Raw s ->
     begin match List.head_opt s.data with
     | Some (_, { timestamp; tables }) -> Some timestamp, tables
     | None -> None, []
     end
     |> Lwt_result.return
  | _ -> Lwt.fail_with "got compressed"

let name = "Обзор таблиц"
let base_class = "qos-niit-table-overview"
let failure_class = Markup.CSS.add_modifier base_class "failure"

let settings = None

module Table_info = struct
  type t = table_info

  type id =
    { id : int
    ; id_ext : int
    ; ext_info : ext_info
    ; pid : int
    } [@@deriving ord]

  let to_id ({ id; id_ext; ext_info; pid; _ } : t) : id =
    { id; id_ext; ext_info; pid }

  let compare (a : t) (b : t) : int =
    compare_id (to_id a) (to_id b)

  let equal (a : t) (b : t) : bool =
    0 = compare a b

end

module Set = Set.Make(Table_info)

(** Returns string representing human-readable section name *)
let to_table_name ?(is_hex = false) table_id table_id_ext
      service (ext_info : ext_info) =
  let open Printf in
  let divider  = ", " in
  let name     = Mpeg_ts.(table_to_string @@ table_of_int table_id) in
  let id ?service s x =
    let s = if is_hex then sprintf "%s=0x%02X" s x
            else sprintf "%s=%02d" s x in
    match service with
    | Some x -> sprintf "%s (%s)" s x
    | None -> s in
  let base     = id "table_id" table_id in
  let specific = match Mpeg_ts.table_of_int table_id with
    | `PAT -> Some [ id "tsid" table_id_ext ]
    | `PMT -> Some [ id ?service "program" table_id_ext ]
    | `NIT _ -> Some [ id "network_id" table_id_ext ]
    | `SDT _ -> Some [ id "tsid" table_id_ext
                     ; id "onid" ext_info.ext_1 ]
    | `BAT -> Some [ id "bid" table_id_ext ]
    | `EIT _ -> Some [ id ?service "sid" table_id_ext
                     ; id "tsid" ext_info.ext_1
                     ; id "onid" ext_info.ext_2 ]
    | _ -> None in
  match specific with
  | Some l -> name, String.concat divider (base :: l)
  | None -> name, base

(** Returns HTML element to insert into 'Extra' table column *)
let to_table_extra ?(hex = false) (x : table_info) =
  let id = match hex with
    | true  -> Printf.sprintf "0x%02X"
    | false -> Printf.sprintf "%d" in
  let specific = match Mpeg_ts.table_of_int x.id with
    | `PAT   -> Some [ "tsid", x.id_ext ]
    | `PMT   -> Some [ "program", x.id_ext ]
    | `NIT _ -> Some [ "network_id", x.id_ext ]
    | `SDT _ -> Some [ "tsid", x.id_ext
                     ; "onid", x.ext_info.ext_1 ]
    | `BAT   -> Some [ "bid", x.id_ext ]
    | `EIT _ -> Some [ "onid", x.ext_info.ext_1
                     ; "tsid", x.ext_info.ext_2
                     ; "sid",  x.id_ext ]
    | _      -> None in
  let open Tyxml_js.Html in
  let wrap x =
    List.mapi (fun i (s, v) ->
        let v = id v in
        let v = if i = pred @@ List.length x then v else v ^ ", " in
        span [ span ~a:[ a_class [ Typography.Markup.subtitle2_class ]]
                 [pcdata (s ^ ": ")]
             ; pcdata v ]) x
    |> fun x -> span x in
  (match specific with
   | Some l -> wrap l
   | None -> span [])
  |> toelt

(** Returns 'back' action element *)
let make_back () =
  let back =
    new Icon_button.t ~icon:Icon.SVG.(create_simple Path.arrow_left) () in
  back#add_class @@ Markup.CSS.add_element base_class "back";
  back

let section_fmt : 'a list Table.custom =
  { is_numeric = true
  ; compare = (fun x y -> Int.compare (List.length x) (List.length y))
  ; to_string = Fun.(string_of_int % List.length) }

let make_table (is_hex : bool)
      (init : table_info list) =
  let open Table in
  let dec_ext_fmt =
    Custom_elt { is_numeric = false
               ; compare = compare_table_info
               ; to_elt = to_table_extra } in
  let hex_ext_fmt =
    Custom_elt { is_numeric = false
               ; compare = compare_table_info
               ; to_elt = to_table_extra ~hex:true } in
  let dec_pid_fmt = Int (Some (Printf.sprintf "%d")) in
  let hex_pid_fmt = Int (Some (Printf.sprintf "0x%04X")) in
  let hex_tid_fmt = Int (Some (Printf.sprintf "0x%02X")) in
  (* let br_fmt = Table.(Option (Float None, "-")) in *)
  let fmt =
    let open Format in
    (to_column ~sortable:true "ID", dec_pid_fmt)
    :: (to_column ~sortable:true "PID", dec_pid_fmt)
    :: (to_column ~sortable:true "Имя", String None)
    :: (to_column "Доп. инфо", dec_ext_fmt)
    :: (to_column ~sortable:true "Версия", Int None)
    :: (to_column ~sortable:true "Сервис", Option (String None, ""))
    :: (to_column "Количество секций", Custom section_fmt)
    :: (to_column "Last section", Int None)
    (* :: (to_column "Битрейт, Мбит/с", br_fmt) *)
    :: [] in
  let table = new t ~dense:true ~fmt () in
  let on_change = fun (x : bool) ->
    List.iter (fun row ->
        match row#cells with
        | tid :: pid :: _ :: ext :: _ ->
           pid#set_format (if x then hex_pid_fmt else dec_pid_fmt);
           tid#set_format (if x then hex_tid_fmt else dec_pid_fmt);
           ext#set_format (if x then hex_ext_fmt else dec_ext_fmt);)
      table#rows in
  if is_hex then on_change true;
  table, on_change

let table_info_to_data (x : table_info) =
  let open Table.Data in
  let name = Mpeg_ts.(table_to_string @@ table_of_int x.id) in
  let sections = List.map (fun x -> x, None) x.sections in
  x.id :: x.pid :: name :: x :: x.version
  :: x.service :: sections :: x.last_section (* :: None *) :: []

let make_dump_title ?is_hex
      ({ id; id_ext; ext_info; service; _ } : table_info) =
  to_table_name ?is_hex id id_ext service ext_info

module Heading = struct

  class t ?title ?subtitle ~meta () =
    let title' = new Card.Primary.title "" () in
    let subtitle' = new Card.Primary.subtitle "" () in
    let box = Widget.create_div () in
    object(self)
      inherit Card.Primary.t ~widgets:[box; meta#widget] ()

      method set_title (s : string) : unit =
        title'#set_text_content s

      method set_subtitle (s : string) : unit =
        subtitle'#set_text_content s

      initializer
        box#append_child title';
        box#append_child subtitle';
        Option.iter self#set_title title;
        Option.iter self#set_subtitle subtitle;
    end

end

let add_row (table : 'a Table.t)
      (stream : Stream.t)
      (primary : Heading.t)
      (media : Card.Media.t)
      (control : int)
      (set_dump : (table_info * Widget_tables_dump.t) option -> unit)
      (x : table_info) =
  let row = table#add_row (table_info_to_data x) in
  row#listen_click_lwt (fun _ _ ->
      let cell =
        let open Table in
        match row#cells with
        | _ :: _ :: _ :: _ :: _ :: _ :: x :: _ -> x in
      let back = make_back () in
      let title, subtitle = make_dump_title x in
      primary#set_title title;
      primary#set_subtitle subtitle;
      let dump =
        new Widget_tables_dump.t
          ~config:{ stream }
          ~sections:cell#value
          ~id:x.id
          ~id_ext:x.id_ext
          ~ext_info:x.ext_info
          control () in
      set_dump @@ Some (x, dump);
      back#listen_click_lwt (fun _ _ ->
          let sections = List.map (fun i -> i#value) dump#list#items in
          cell#set_value ~force:true sections;
          media#set_empty ();
          media#append_child table;
          primary#remove_child back;
          set_dump None;
          dump#destroy ();
          Lwt.return_unit) |> Lwt.ignore_result;
      begin match dump#list#items with
      | hd :: _ -> dump#list#set_active hd
      | _ -> ()
      end;
      primary#insert_child_at_idx 0 back;
      media#remove_child table;
      media#append_child dump;
      Lwt.return_unit)
  |> Lwt.ignore_result;
  row

let make_timestamp_string (timestamp : Time.t option) =
  let tz_offset_s = Ptime_clock.current_tz_offset_s () in
  let s = match timestamp with
    | None -> "-"
    | Some t -> Time.to_human_string ?tz_offset_s t
  in
  "Обновлено: " ^ s

class t (stream : Stream.t)
        (timestamp : Time.t option)
        (init : table_info list)
        (control : int)
        () =
  (* FIXME should remember preffered state *)
  let is_hex = false in
  let table, on_change = make_table is_hex init in
  let title = "Обзор" in
  let subtitle = make_timestamp_string timestamp in
  let dump, set_dump = React.S.create None in
  let on_change = fun x ->
    (* FIXME change dump title too *)
    on_change x in
  let hex = let switch = new Switch.t ~state:is_hex ~on_change () in
            new Form_field.t ~input:switch ~label:"HEX IDs" () in
  let primary = new Heading.t ~title ~subtitle ~meta:hex () in
  object(self)

    val mutable _timestamp : Time.t option = timestamp
    val mutable _data : Set.t = Set.of_list init
    val media = new Card.Media.t ~widgets:[table] ()

    inherit Card.t ~widgets:[ ] ()

    (** Adds new row to the overview *)
    method add_row (t : table_info) =
      add_row table stream primary media control set_dump t

    method set_sync (x : bool) : unit =
      self#add_or_remove_class (not x) failure_class

    method set_state (x : Topology.state) : unit =
      ()

    (** Updates bitrate values *)
    method set_rate (x : bitrate) : unit =
      let rec aux = function
        | [], _ -> ()
        | _, [] -> ()
        | (rate : table_bitrate) :: tl, rows ->
           (* let val_float = Float.(of_int rate.bitrate / 1_000_000.) in *)
           let find (row : 'a Table.Row.t) =
             let x : table_info = self#_row_to_table_info row in
             x.id = rate.id
             && x.id_ext = rate.id_ext
             && x.ext_info.ext_1 = rate.ext_info_1
             && x.ext_info.ext_2 = rate.ext_info_2 in
           match List.find_opt find rows with
           | None ->
              aux (tl, rows)
           | Some row ->
              (* let open Table in
               * let rate' = match row#cells with
               *   | _ :: _ :: _ :: _ :: _ :: _ :: _ :: _ :: a :: _ ->
               *      a in
               * rate'#set_value @@ Some val_float; *)
              let rows = List.remove ~eq:Widget.equal ~x:row rows in
              aux (tl, rows)
      in
      aux (x.tables, table#rows)

    (** Updates the overview *)
    method update ({ timestamp; tables } : tables) =
      (* Update timestamp *)
      _timestamp <- Some timestamp;
      (* Set timestamp in a heading only if dump view is not active *)
      begin match React.S.value dump with
      | None -> primary#set_subtitle @@ make_timestamp_string _timestamp;
      | Some _ -> ()
      end;
      (* Manage found, lost and updated items *)
      let prev = _data in
      _data <- Set.of_list tables;
      let lost = Set.diff prev _data in
      let found = Set.diff _data prev in
      let inter = Set.inter prev _data in
      let upd = Set.filter (fun (x : table_info) ->
                    List.mem ~eq:equal_table_info x tables) inter in
      let find = fun (table : table_info) (row : 'a Table.Row.t) ->
        let open Table in
        let info = self#_row_to_table_info row in
        Table_info.equal table info in
      Set.iter (fun (info : table_info) ->
          match List.find_opt (find info) table#rows with
          | None -> ()
          | Some row -> table#remove_row row) lost;
      Set.iter (fun (info : table_info) ->
          match List.find_opt (find info) table#rows with
          | None -> ()
          | Some row -> self#_update_row row info) upd;
      Set.iter (ignore % self#add_row) found

    (* Private methods *)

    method private _update_row (row : 'a Table.Row.t) (x : table_info) =
      let open Table in
      begin match row#cells with
      | _ :: _ :: _ :: _ :: ver :: serv :: sect :: lsn :: _ ->
         let sections =
           List.map (fun x ->
               let res = List.find_opt (equal_section_info x % fst)
                           sect#value in
               match  res with
               | Some (_, dump) -> x, dump
               | None -> x, None) x.sections in
         ver#set_value x.version;
         serv#set_value x.service;
         sect#set_value sections;
         lsn#set_value x.last_section
      end

    method private _row_to_table_info (row : 'a Table.Row.t) =
      let open Table in
      match row#cells with
      | _ :: _ :: _ :: x :: _ -> x#value

    initializer
      self#_keep_e
      @@ React.E.map (function
             | Some _ -> ()
             | None ->
                primary#set_title title;
                primary#set_subtitle @@ make_timestamp_string _timestamp)
      @@ React.S.changes dump;
      Set.iter (ignore % self#add_row) _data;
      self#add_class base_class;
      self#append_child primary;
      self#append_child @@ new Divider.t ();
      self#append_child media;
  end

let make ?(init : tables option)
      (stream : Stream.t)
      control =
  let init = match init with
    | Some x -> Lwt_result.return (Some x.timestamp, x.tables)
    | None -> get_tables ~id:stream.id control in
  init
  >|= (fun (ts, data) -> new t stream ts data control ())
  |> Ui_templates.Loader.create_widget_loader
