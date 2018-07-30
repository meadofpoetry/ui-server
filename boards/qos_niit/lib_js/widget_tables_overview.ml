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

let to_table_extra ?(hex=false) (x:table_info) =
  let id = match hex with
    | true  -> Printf.sprintf "0x%02X"
    | false -> Printf.sprintf "%d" in
  let specific = match table_of_int x.id with
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
        span [ span ~a:[ a_class [ Typography.Markup.body2_class ]]
                 [pcdata (s ^ ": ")]
             ; pcdata v ]) x
    |> fun x -> span x in
  (match specific with
   | Some l -> wrap l
   | None   -> span [])
  |> Tyxml_js.Html.toelt

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
  let table_info_to_data (x:table_info) =
    let open Table.Data in
    let name = table_to_string @@ table_of_int x.id in
    x.pid :: x.id :: name :: x :: x.version
    :: x.service :: x.section :: x.last_section :: [] in
  let fmt =
    let open Table in
    let open Format in
    (   to_column ~sortable:true "PID",     if is_hex
                                            then hex_pid_fmt else dec_pid_fmt)
    :: (to_column ~sortable:true "ID",      if is_hex
                                            then hex_tid_fmt else dec_pid_fmt)
    :: (to_column ~sortable:true "Имя",     String None)
    :: (to_column "Доп. инфо",              if is_hex
                                            then hex_ext_fmt else dec_ext_fmt)
    :: (to_column ~sortable:true "Версия",  Int None)
    :: (to_column ~sortable:true "Сервис",  Option (String None, ""))
    :: (to_column "Section",                Int None)
    :: (to_column "Last section",           Int None)
    :: [] in
  let table = new Table.t ~sticky_header:true ~dense:true ~fmt () in
  let dump  = new Widget_tables_dump.t
                ~config:{ stream = config.stream }
                ~init ~event:React.E.never control () in
  let on_change = fun (x:bool) ->
    List.iter (fun row ->
        let open Table in
        match row#cells with
        | pid :: tid :: _ :: ext :: _ ->
           pid#set_format (if x then hex_pid_fmt else dec_pid_fmt);
           tid#set_format (if x then hex_tid_fmt else dec_pid_fmt);
           ext#set_format (if x then hex_ext_fmt else dec_ext_fmt);)
      table#rows in
  let back_ico = new Icon.Button.Font.t ~icon:"arrow_back" () in
  let back_txt = new Typography.Text.t ~adjust_margin:false
                   ~font:Caption
                   ~text:"Назад" () in
  let back   = new Hbox.t
                 ~valign:`Center
                 ~widgets:[ back_ico#widget; back_txt#widget ] () in
  let ()     = back#add_class @@ Markup.CSS.add_element base_class "back" in
  let switch = new Switch.t ~state:is_hex ~on_change () in
  let hex    = new Form_field.t ~input:switch ~label:"HEX IDs" () in
  let actions  =
    new Card.Actions.t ~widgets:[ back#widget; hex#widget ] () in
  let media  = new Card.Media.t ~widgets:[ table ] () in
  let card =
    new Card.t ~widgets:[ actions#widget
                        ; (new Divider.t ())#widget
                        ; media#widget ] () in
  back#listen Widget.Event.click (fun _ _ ->
      media#append_child table;
      media#remove_child dump;
      hex#style##.visibility  := Js.string "";
      back#style##.visibility := Js.string "hidden";
      true) |> ignore;
  back#style##.visibility := Js.string "hidden";
  let add_row (x:table_info) =
    let row = table#add_row (table_info_to_data x) in
    row#listen Widget.Event.click (fun _ _ ->
        back#style##.visibility := Js.string "";
        hex#style##.visibility  := Js.string "hidden";
        media#remove_child table;
        media#append_child dump;
        (match List.find_opt (fun i -> equal_table_info (fst i#value) x)
                 dump#tables#items with
         | Some item -> dump#tables#set_active item;
                        item#root##scrollIntoView Js._true;
         | None      -> ());
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
        | Some (_, tables) -> tables.tables
        | None -> [] in
      make_table ~config tables control)
    >|= Widget.coerce
    |> Lwt_result.map_err Api_js.Requests.err_to_string
    |> Ui_templates.Loader.create_widget_loader
  in loader


