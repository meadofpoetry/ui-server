open Containers
open Components
open Common
open Board_types.Streams.TS
open Lwt_result.Infix
open Api_js.Api_types

type config =
  { stream : Stream.t
  }

let name = "Обзор таблиц"
let base_class = "qos-niit-table-overview"

let settings = None

let to_table_name table_id table_id_ext (eit_params:eit_params) =
  let divider  = ", " in
  let name     = Mpeg_ts.(table_to_string @@ table_of_int table_id) in
  let id s x   = Printf.sprintf "%s=0x%02X(%d)" s x x in
  let base     = id "table_id" table_id in
  let specific = match Mpeg_ts.table_of_int table_id with
    | `PAT   -> Some [ id "tsid" table_id_ext ]
    | `PMT   -> Some [ id "program" table_id_ext ]
    | `NIT _ -> Some [ id "network_id" table_id_ext ]
    | `SDT _ -> Some [ id "tsid" table_id_ext ]
    | `BAT   -> Some [ id "bid" table_id_ext ]
    | `EIT _ -> Some [ id "sid" table_id_ext
                     ; id "tsid" eit_params.ts_id
                     ; id "onid" eit_params.orig_nw_id ]
    | _      -> None in
  match specific with
  | Some l -> name, String.concat divider (base :: l)
  | None   -> name, base

let to_table_extra ?(hex=false) (x:table_info) =
  let id = match hex with
    | true  -> Printf.sprintf "0x%02X"
    | false -> Printf.sprintf "%d" in
  let specific = match Mpeg_ts.table_of_int x.id with
    | `PAT   -> Some [ "tsid", x.id_ext ]
    | `PMT   -> Some [ "program", x.id_ext ]
    | `NIT _ -> Some [ "network_id", x.id_ext ]
    | `SDT _ -> Some [ "tsid", x.id_ext ]
    | `BAT   -> Some [ "bid", x.id_ext ]
    | `EIT _ -> Some [ "onid", x.eit_params.orig_nw_id
                     ; "tsid", x.eit_params.ts_id
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
   | None   -> span [])
  |> Tyxml_js.Html.toelt

let make_back () =
  let back_ico =
    new Icon.Button.Font.t ~icon:"arrow_back" () in
  let back_txt =
    new Typography.Text.t ~adjust_margin:false
      ~font:Caption
      ~text:"Назад" () in
  let back =
    new Hbox.t
      ~valign:`Center
      ~widgets:[ back_ico#widget; back_txt#widget ] () in
  let () = back#add_class @@ Markup.CSS.add_element base_class "back" in
  back

let make_dump_title ({ id; id_ext; eit_params; _ }:table_info) =
  let name  = to_table_name id id_ext eit_params in
  let title = new Card.Primary.title (fst name) () in
  let subtitle = new Card.Primary.subtitle (snd name) () in
  new Card.Primary.t ~widgets:[ title; subtitle ] ()

let make_table
      ~config
      (init:table_info list)
      control =
  (* FIXME should remember preffered state *)
  let is_hex = false in
  let dec_ext_fmt = Table.(Custom_elt { is_numeric = false
                                      ; compare = compare_table_info
                                      ; to_elt = to_table_extra }) in
  let hex_ext_fmt = Table.(Custom_elt { is_numeric = false
                                      ; compare = compare_table_info
                                      ; to_elt = to_table_extra ~hex:true }) in
  let dec_pid_fmt = Table.(Int (Some (Printf.sprintf "%d"))) in
  let hex_pid_fmt = Table.(Int (Some (Printf.sprintf "0x%04X"))) in
  let hex_tid_fmt = Table.(Int (Some (Printf.sprintf "0x%02X"))) in
  let section_fmt = Table.(
      Custom { is_numeric = true
             ; compare    = (fun x y -> Int.compare
                                          (List.length x)
                                          (List.length y))
             ; to_string  = Fun.(string_of_int % List.length) }) in
  let table_info_to_data (x:table_info) =
    let open Table.Data in
    let name = Mpeg_ts.(table_to_string @@ table_of_int x.id) in
    let sections = List.map (fun x -> x, None) x.sections in
    x.id :: x.pid :: name :: x :: x.version
    :: x.service :: sections :: x.last_section :: [] in
  let fmt =
    let open Table in
    let open Format in
    (   to_column ~sortable:true "ID",      if is_hex
                                            then hex_tid_fmt else dec_pid_fmt)
    :: (to_column ~sortable:true "PID",     if is_hex
                                            then hex_pid_fmt else dec_pid_fmt)
    :: (to_column ~sortable:true "Имя",     String None)
    :: (to_column "Доп. инфо",              if is_hex
                                            then hex_ext_fmt else dec_ext_fmt)
    :: (to_column ~sortable:true "Версия",  Int None)
    :: (to_column ~sortable:true "Сервис",  Option (String None, ""))
    :: (to_column "Количество секций",      section_fmt)
    :: (to_column "Last section",           Int None)
    :: [] in
  let table = new Table.t ~sticky_header:true ~dense:true ~fmt () in
  let on_change = fun (x:bool) ->
    List.iter (fun row ->
        let open Table in
        match row#cells with
        | tid :: pid :: _ :: ext :: _ ->
           pid#set_format (if x then hex_pid_fmt else dec_pid_fmt);
           tid#set_format (if x then hex_tid_fmt else dec_pid_fmt);
           ext#set_format (if x then hex_ext_fmt else dec_ext_fmt);)
      table#rows in
  let switch = new Switch.t ~state:is_hex ~on_change () in
  let hex    = new Form_field.t ~input:switch ~label:"HEX IDs" () in
  let actions  =
    new Card.Actions.t ~widgets:[ hex#widget ] () in
  let media  = new Card.Media.t ~widgets:[ table ] () in
  let card =
    new Card.t ~widgets:[ actions#widget
                        ; (new Divider.t ())#widget
                        ; media#widget ] () in
  let add_row (x:table_info) =
    let row = table#add_row (table_info_to_data x) in
    row#listen Widget.Event.click (fun _ _ ->
        let cell =
          let open Table in
          match row#cells with
          | _ :: _ :: _ :: _ :: _ :: _ :: x :: _ -> x in
        let back    = make_back () in
        let title   = make_dump_title x in
        let divider = new Divider.t () in
        let dump =
          new Widget_tables_dump.t
            ~config:{ stream = config.stream }
            ~sections:(List.filter_map (fun (x, v) ->
                           match v with
                           | Some v -> Some (x, v)
                           | None   -> None) cell#value)
            ~init:x
            ~event:React.E.never
            control () in
        back#listen Widget.Event.click (fun _ _ ->
            let sections = List.map (fun i -> i#value) dump#list#items in
            cell#set_value sections;
            card#remove_child title;
            card#remove_child divider;
            media#set_empty ();
            media#append_child table;
            hex#style##.visibility  := Js.string "";
            actions#remove_child back;
            true) |> ignore;
        (match dump#list#items with
         | hd :: _ -> dump#list#set_active hd
         | _       -> ());
        card#insert_child_at_idx 2 title;
        card#insert_child_at_idx 3 divider;
        actions#insert_child_at_idx 0 back;
        hex#style##.visibility  := Js.string "hidden";
        media#remove_child table;
        media#append_child dump;
        true) |> ignore in
  List.iter add_row init;
  card#add_class base_class;
  card#widget

let make ~(config:config) control =
  let id   = match config.stream.id with
    | `Ts id -> id
    | `Ip _  -> failwith "UDP" in
  let init =
    Requests.Streams.HTTP.get_tables ~id ~limit:1 control
    >>= (function
         | Raw s -> Lwt_result.return s.data
         | _     -> Lwt.fail_with "got compressed") in
  let loader =
    init
    >|= (fun init ->
      let tables = match List.head_opt init with
        | Some (_, tables) ->
           let list = tables.tables in
           List.sort compare_table_info list
        | None -> [] in
      make_table ~config tables control)
    >|= Widget.coerce
    |> Lwt_result.map_err Api_js.Requests.err_to_string
    |> Ui_templates.Loader.create_widget_loader
  in loader


