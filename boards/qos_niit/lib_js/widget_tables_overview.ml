open Containers
open Components
open Board_types
open Widget_common
open Common

let name = "Обзор таблиц"
let base_class = "qos-niit-table-overview"
let no_sync_class = Markup.CSS.add_modifier base_class "no-sync"
let no_response_class = Markup.CSS.add_modifier base_class "no-response"

module Settings = struct
  type t = { hex : bool } [@@deriving eq]

  let (default : t) = { hex = false }

  let make_hex_swith state =
    let input = new Switch.t ~state () in
    new Form_field.t ~input ~align_end:true ~label:"HEX IDs" ()

  class view ?(settings = default) () =
    let hex_switch = make_hex_swith settings.hex in
    object
      val mutable value = settings
      inherit Vbox.t ~widgets:[hex_switch] ()
      method value : t = value
      method apply () : unit =
        let hex = hex_switch#input_widget#checked in
        value <- { hex }
      method reset () : unit =
        let { hex } = value in
        hex_switch#input_widget#set_checked hex
    end

  let make ?settings () = new view ?settings ()

end

module Table_info = struct

  include SI_PSI_table

  let compare (a : t) (b : t) : int =
    compare_id (fst a) (fst b)

  let equal (a : t) (b : t) : bool =
    0 = compare a b

end

module Set = Set.Make(Table_info)

(** Returns string representing human-readable section name *)
let to_table_name ?(is_hex = false) table_id table_id_ext
      id_ext_1 id_ext_2 service =
  let open Printf in
  let divider = ", " in
  let name = Mpeg_ts.(table_to_string @@ table_of_int table_id) in
  let id ?service s x =
    let s = if is_hex then sprintf "%s=0x%02X" s x
            else sprintf "%s=%02d" s x in
    match service with
    | None -> s
    | Some x -> sprintf "%s (%s)" s x in
  let base = id "table_id" table_id in
  let specific = match Mpeg_ts.table_of_int table_id with
    | `PAT -> [id "tsid" table_id_ext]
    | `PMT -> [id ?service "program" table_id_ext]
    | `NIT _ -> [id "network_id" table_id_ext]
    | `SDT _ -> [ id "tsid" table_id_ext
                ; id "onid" id_ext_1 ]
    | `BAT -> [id "bid" table_id_ext]
    | `EIT _ -> [ id ?service "sid" table_id_ext
                ; id "tsid" id_ext_1
                ; id "onid" id_ext_2 ]
    | _ -> [] in
  name, String.concat divider (base :: specific)

(** Returns HTML element to insert into 'Extra' table column *)
let to_table_extra ?(hex = false) ((id, _) : SI_PSI_table.t) =
  let to_id_string = match hex with
    | true  -> Printf.sprintf "0x%02X"
    | false -> Printf.sprintf "%d" in
  let specific = match Mpeg_ts.table_of_int id.table_id with
    | `PAT -> Some ["tsid", id.table_id_ext]
    | `PMT -> Some ["program", id.table_id_ext]
    | `NIT _ -> Some ["network_id", id.table_id_ext]
    | `SDT _ -> Some [ "tsid", id.table_id_ext
                     ; "onid", id.id_ext_1 ]
    | `BAT -> Some ["bid", id.table_id_ext]
    | `EIT _ -> Some [ "onid", id.id_ext_1
                     ; "tsid", id.id_ext_2
                     ; "sid",  id.table_id_ext ]
    | _ -> None in
  Tyxml_js.Html.(
    let wrap x =
      List.mapi (fun i (s, v) ->
          let v = to_id_string v in
          let v = if i = pred @@ List.length x then v else v ^ ", " in
          span [ span ~a:[a_class [ Typography.Markup.subtitle2_class]]
                   [txt (s ^ ": ")]
               ; txt v ]) x
      |> span in
    toelt @@ match specific with
             | None -> span []
             | Some l -> wrap l)

(** Returns 'back' action element *)
let make_back () =
  let icon = Icon.SVG.(create_simple Path.arrow_left) in
  let back = new Icon_button.t ~icon () in
  back#add_class @@ Markup.CSS.add_element base_class "back";
  back

let section_fmt : 'a. unit -> 'a list Components.Table.custom =
  fun () ->
  { is_numeric = true
  ; compare = (fun x y -> List.(Int.compare (length x) (length y)))
  ; to_string = Fun.(string_of_int % List.length)
  }

let make_table ?(is_hex = false) () =
  let open Table in
  let dec_ext_fmt =
    Custom_elt { is_numeric = false
               ; compare = SI_PSI_table.compare
               ; to_elt = to_table_extra } in
  let hex_ext_fmt =
    Custom_elt { is_numeric = false
               ; compare = SI_PSI_table.compare
               ; to_elt = to_table_extra ~hex:true } in
  let dec_pid_fmt = Int (Some (Printf.sprintf "%d")) in
  let hex_pid_fmt = Int (Some (Printf.sprintf "0x%04X")) in
  let hex_tid_fmt = Int (Some (Printf.sprintf "0x%02X")) in
  let br_fmt = Option (Float None, "-") in
  let pct_fmt = Option (Float (Some (Printf.sprintf "%.2f")), "-") in
  let fmt =
    let open Format in
    (to_column ~sortable:true "ID", dec_pid_fmt)
    :: (to_column ~sortable:true "PID", dec_pid_fmt)
    :: (to_column ~sortable:true "Имя", String None)
    :: (to_column "Доп. инфо", dec_ext_fmt)
    :: (to_column ~sortable:true "Версия", Int None)
    :: (to_column ~sortable:true "Сервис", Option (String None, ""))
    :: (to_column "Кол-во секций", Custom (section_fmt ()))
    :: (to_column "LSN", Int None)
    :: (to_column "Битрейт, Мбит/с", br_fmt)
    :: (to_column "%", pct_fmt)
    :: (to_column "Min, Мбит/с", br_fmt)
    :: (to_column "Max, Мбит/с", br_fmt)
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

let table_info_to_data ((id, info) : SI_PSI_table.t) =
  let open Table.Data in
  let name = Mpeg_ts.(table_to_string @@ table_of_int id.table_id) in
  let sections = List.map (fun x -> x, None) info.sections in
  id.table_id :: info.pid :: name :: (id, info) :: info.version
  :: info.service_name :: sections :: info.last_section
  :: None :: None :: None :: None :: []

let make_dump_title ?is_hex
      (({ table_id; table_id_ext; id_ext_1; id_ext_2; _ } : SI_PSI_table.id),
       ({ service_name; _ } : SI_PSI_table.info)) =
  to_table_name ?is_hex table_id table_id_ext id_ext_1 id_ext_2 service_name

module Heading = struct

  class t ?meta ?title ?subtitle () =
    let title' = new Card.Primary.title "" () in
    let subtitle' = new Card.Primary.subtitle "" () in
    let box = Widget.create_div ~widgets:[title'; subtitle'] () in
    let widgets = List.cons_maybe meta [box] in
    object(self)
      inherit Card.Primary.t ~widgets () as super

      method! init () : unit =
        super#init ();
        Option.iter self#set_title title;
        Option.iter self#set_subtitle subtitle;

      method set_title (s : string) : unit =
        title'#set_text_content s

      method set_subtitle (s : string) : unit =
        subtitle'#set_text_content s
    end

end

let add_row (table : 'a Table.t)
      (is_hex : bool)
      (stream : Stream.ID.t)
      (parent : #Widget.t)
      (control : int)
      (set_dump : ((bool -> unit) * Widget_tables_dump.t) option -> unit)
      ((id, info) : SI_PSI_table.t) =
  let row = table#push (table_info_to_data (id, info)) in
  let listener =
    row#listen_click_lwt (fun _ _ ->
        let open Lwt.Infix in
        let cell =
          let open Table in
          match row#cells with
          | _ :: _ :: _ :: _ :: _ :: _ :: x :: _ -> x in
        let back = make_back () in
        let title, subtitle = make_dump_title ~is_hex (id, info) in
        let heading = new Heading.t ~meta:back#widget ~title ~subtitle () in
        let dump =
          new Widget_tables_dump.t
            ~stream
            ~sections:cell#value
            ~table_id:id.table_id
            ~table_id_ext:id.table_id_ext
            ~id_ext_1:id.id_ext_1
            ~id_ext_2:id.id_ext_2
            control () in
        let set_hex (x : bool) =
          let title, subtitle = make_dump_title (id, info) in
          heading#set_title title;
          heading#set_subtitle subtitle;
          dump#set_hex x in
        let box =
          new Vbox.t
            ~widgets:[ heading#widget
                     ; (new Divider.t ())#widget
                     ; dump#widget ]
            () in
        dump#set_hex is_hex;
        set_dump @@ Some (set_hex, dump);
        back#listen_once_lwt Widget.Event.click
        >|= (fun _ ->
          let sections = List.map (fun i -> i#value) dump#list#items in
          Printf.printf "got %d dumped\n" @@ List.length @@ List.filter (fun (_, d) -> Option.is_some d) sections;
          cell#set_value ~force:true sections;
          parent#remove_child box;
          parent#append_child table;
          set_dump None;
          dump#destroy ();
          back#destroy ())
        |> Lwt.ignore_result;
        begin match dump#list#items with
        | [] -> ()
        | hd :: _ -> dump#list#set_active hd
        end;
        parent#remove_child table;
        parent#append_child box;
        Lwt.return_unit) in
  row#set_on_destroy (fun () -> Lwt.cancel listener);
  row

class t ?(settings : Settings.t option)
        (init : SI_PSI_table.t list Time.timestamped option)
        (stream : Stream.ID.t)
        (control : int)
        () =
  let init, timestamp = match init with
    | None -> [], None
    | Some { data; timestamp } -> data, Some timestamp in
  let s_time, set_time =
    React.S.create ~eq:(Equal.option Time.equal) timestamp in
  let table, on_change = make_table () in
  let dump, set_dump =
    let eq = fun (_, w1) (_, w2) -> Widget.equal w1 w2 in
    React.S.create ~eq:(Equal.option eq) None in
  let empty =
    Ui_templates.Placeholder.create_with_icon
      ~icon:Icon.SVG.(create_simple Path.emoticon_sad)
      ~text:"Не найдено ни одной таблицы SI/PSI"
      () in

  object(self)

    val mutable _hex : bool = false
    val mutable _data : Set.t = Set.of_list init
    val media = new Card.Media.t ~widgets:[table] ()

    inherit Widget.t Js_of_ocaml.Dom_html.(createDiv document) () as super

    method! init () : unit =
      super#init ();
      Option.iter self#set_settings settings;
      self#append_child table;
      React.S.map ~eq:Equal.unit (function
          | [] -> self#append_child empty
          | _ -> self#remove_child empty) table#s_rows
      |> self#_keep_s;
      Set.iter Fun.(ignore % self#add_row) _data;
      self#add_class base_class

    method s_timestamp : Time.t option React.signal =
      s_time

    (** Adds new row to the overview *)
    method add_row (t : SI_PSI_table.t) : unit =
      ignore @@ add_row table self#hex stream self#widget control set_dump t

    (** Updates widget state *)
    method set_state (x : widget_state) : unit =
      begin match React.S.value dump with
      | Some _ -> () (* FIXME !!!!!!!! set state *)
      | None -> ()
      end;
      match x with
      | Fine ->
         self#remove_class no_response_class;
         self#remove_class no_sync_class
      | No_sync ->
         self#remove_class no_response_class;
         self#add_class no_sync_class
      | No_response ->
         self#remove_class no_sync_class;
         self#add_class no_response_class

    method hex : bool = _hex

    (** Sets identifiers to hex or decimal view *)
    method set_hex (x : bool) : unit =
      _hex <- x;
      begin match React.S.value dump with
      | None -> ()
      | Some (set, _) -> set x
      end;
      on_change x

    method set_settings (x : Settings.t) : unit =
      self#set_hex x.hex

    (** Updates bitrate values *)
    method set_rate (x : Bitrate.t) : unit =
      let rec aux = function
        | [], _ -> ()
        | _, [] -> ()
        | (rate : Bitrate.table) :: tl, rows ->
           let find (row : 'a Table.Row.t) =
             let ((id, _) : SI_PSI_table.t) = self#_row_to_table_info row in
             id.table_id = rate.table_id
             && id.table_id_ext = rate.table_id_ext
             && id.id_ext_1 = rate.id_ext_1
             && id.id_ext_2 = rate.id_ext_2 in
           match List.find_opt find rows with
           | None ->
              aux (tl, rows)
           | Some row ->
              let open Table in
              let rate', pct', min', max' = match row#cells with
                | _ :: _ :: _ :: _ :: _ :: _ :: _ :: _
                  :: rate :: pct :: min :: max :: [] ->
                   rate, pct, min, max in
              let br =
                Float.(of_int rate.bitrate / 1_000_000.) in
              let pct =
                Float.(100. * (of_int rate.bitrate) / (of_int x.total)) in
              rate'#set_value @@ Some br;
              pct'#set_value @@ Some pct;
              begin match min'#value with
              | None -> min'#set_value (Some br)
              | Some v -> if br <. v then min'#set_value (Some br)
              end;
              begin match max'#value with
              | None -> max'#set_value (Some br)
              | Some v -> if br >. v then max'#set_value (Some br)
              end;
              let rows = List.remove ~eq:Widget.equal row rows in
              aux (tl, rows)
      in
      aux (x.tables, table#rows)

    (** Updates the overview *)
    method update ({ timestamp; data } : SI_PSI_table.t list Time.timestamped) =
      let open SI_PSI_table in
      (* Update timestamp *)
      set_time @@ Some timestamp;
      (* Manage found, lost and updated items *)
      let prev = _data in
      _data <- Set.of_list data;
      let lost = Set.diff prev _data in
      let found = Set.diff _data prev in
      let inter = Set.inter _data prev in
      let upd =
        Set.filter (fun (t : SI_PSI_table.t) ->
            let (_, i) = Set.find t prev in
            not @@ SI_PSI_table.equal_info (snd t) i)
          inter in
      let find = fun (table : t) (row : 'a Table.Row.t) ->
        let info = self#_row_to_table_info row in
        Table_info.equal table info in
      Set.iter (fun (info : t) ->
          match List.find_opt (find info) table#rows with
          | None -> ()
          | Some row -> table#remove_row row) lost;
      Set.iter (fun (info : t) ->
          match List.find_opt (find info) table#rows with
          | None -> ()
          | Some row -> self#_update_row row info) upd;
      Set.iter self#add_row found

    (* Private methods *)

    method private _update_row (row : 'a Table.Row.t) ((_, info) : SI_PSI_table.t) =
      let open Table in
      let open SI_PSI_table in
      begin match row#cells with
      | _ :: _ :: _ :: _ :: ver :: serv :: sect :: lsn :: _ ->
         let sections =
           List.map (fun x ->
               List.find_opt Fun.(equal_section_info x % fst) sect#value
               |> function
                 | Some (_, dump) -> x, dump
                 | None -> x, None) info.sections in
         ver#set_value info.version;
         serv#set_value info.service_name;
         sect#set_value sections;
         lsn#set_value info.last_section
      end

    method private _row_to_table_info (row : 'a Table.Row.t) =
      let open Table in
      match row#cells with
      | _ :: _ :: _ :: x :: _ -> x#value
  end

let make ?(settings : Settings.t option)
      (init : SI_PSI_table.t list Time.timestamped option)
      (stream : Stream.ID.t)
      control =
  new t ?settings init stream control ()

let make_dashboard_item ?settings init stream control : 'a Dashboard.Item.item =
  let w = make ?settings init stream control in
  let settings = Settings.make ?settings () in
  let (settings : Dashboard.Item.settings) =
    Dashboard.Item.make_settings
      ~widget:settings
      ~set:(fun () -> Lwt_result.return @@ w#set_settings settings#value)
      () in
  let tz_offset_s = Ptime_clock.current_tz_offset_s () in
  let timestamp =
    Dashboard.Item.make_timestamp
      ~time:w#s_timestamp
      ~to_string:(Time.to_human_string ?tz_offset_s)
      () in
  Dashboard.Item.make_item
    ~name:"Список таблиц SI/PSI"
    ~subtitle:(Timestamp timestamp)
    ~settings
    w
