open Containers
open Components
open Common
open Board_types.Streams.TS
open Lwt_result.Infix
open Api_js.Api_types
open Ui_templates.Sdom

type config =
  { stream : Stream.t
  }

let name = "Сервисы"

let settings = None

let base_class = "qos-niit-services-overview"

let make_menu_list () =
  let open Item_list in
  let info = new Item.t ~value:`Info ~text:"Информация о сервисе" () in
  let pids = new Item.t ~value:`Pids ~text:"PIDs" () in
  let list =
    new t
      ~dense:true
      ~items:[ `Item info; `Item pids ] () in
  list#add_class @@ Components_markup.CSS.add_element base_class "menu";
  list

let make_list_title title =
  let text = new Typography.Text.t ~text:title () in
  text#add_class "mdc-list-title";
  text

let make_general_info () =
  let make_item title =
    let meta = new Typography.Text.t ~text:"" () in
    let item = new Item_list.Item.t ~text:title ~meta () in
    item, fun ?(hex=false) x ->
          let s = match hex with
            | false -> Printf.sprintf "%04d" x
            | true  -> Printf.sprintf "0x%04X" x in
          meta#set_text s in
  let id, set_id   = make_item "Service ID" in
  let pmt, set_pmt = make_item "PMT PID" in
  let pcr, set_pcr = make_item "PCR PID" in
  let list =
    new Item_list.t
      ~dense:true
      ~non_interactive:true
      ~items:[ `Item (id ~value:())
             ; `Item (pmt ~value:())
             ; `Item (pcr ~value:()) ] () in
  list, fun ?hex (x:service_info) ->
        set_id  ?hex x.id;
        set_pmt ?hex x.pmt_pid;
        set_pcr ?hex x.pcr_pid

let make_sdt_info () =
  let open Item_list in
  let ok_class  = Components_markup.CSS.add_modifier "mdc-icon" "ok" in
  let err_class = Components_markup.CSS.add_modifier "mdc-icon" "error" in
  let name, set_name =
    let meta = new Typography.Text.t ~text:"" () in
    let item = new Item.t ~value:() ~text:"Имя" ~meta () in
    item, fun x -> meta#set_text x in
  let prov, set_prov =
    let meta = new Typography.Text.t ~text:"" () in
    let item = new Item.t ~value:() ~text:"Провайдер" ~meta () in
    item, fun x -> meta#set_text x in
  let typ, set_typ =
    let meta = new Typography.Text.t ~text:"" () in
    let item = new Item.t ~value:() ~text:"Тип" ~meta () in
    item, fun x -> meta#set_text @@ Mpeg_ts.service_type_to_string x in
  let eit_s, set_eit_s =
    let meta = Icon.SVG.(create_simple Path.check_circle) in
    let item = new Item.t ~value:() ~text:"EIT schedule" ~meta () in
    item, function
    | true  -> meta#add_class ok_class;
               meta#path#set Icon.SVG.Path.check_circle
    | false -> meta#add_class err_class;
               meta#path#set Icon.SVG.Path.close_circle in
  let scr, set_scr =
    let meta = Icon.SVG.(create_simple Path.check_circle) in
    let item = new Item.t ~value:() ~text:"Скремблирование" ~meta () in
    item, function
    | true  -> meta#add_class ok_class;
               meta#path#set Icon.SVG.Path.check_circle
    | false -> meta#add_class err_class;
               meta#path#set Icon.SVG.Path.close_circle in
  let eit_pf, set_eit_pf =
    let meta = Icon.SVG.(create_simple Path.check_circle) in
    let item = new Item.t ~value:() ~text:"EIT P/F" ~meta () in
    item, function
    | true  -> meta#add_class ok_class;
               meta#path#set Icon.SVG.Path.check_circle
    | false -> meta#add_class err_class;
               meta#path#set Icon.SVG.Path.close_circle in
  let running_status, set_running_status =
    let meta = new Typography.Text.t ~text:"" () in
    let item = new Item.t ~value:() ~text:"Running status" ~meta () in
    item, fun x -> meta#set_text @@ Mpeg_ts.running_status_to_string x in
  let list =
    new Item_list.t
      ~dense:true
      ~non_interactive:true
      ~items:[ `Item name
             ; `Item prov
             ; `Item typ
             ; `Item eit_s
             ; `Item scr
             ; `Item eit_pf
             ; `Item running_status ]
      () in
  list, fun (x:service_info) ->
        set_name           x.name;
        set_prov           x.provider_name;
        set_typ            x.service_type;
        set_eit_s          x.eit_schedule;
        set_scr            x.free_ca_mode;
        set_eit_pf         x.eit_pf;
        set_running_status x.running_status

let make_info () =
  let _class = Markup.CSS.add_element base_class "info" in
  let main, set_main = make_general_info () in
  let sdt,  set_sdt  = make_sdt_info () in
  let box = Widget.create_div () in
  let ()  = box#append_child (make_list_title "Общая информация") in
  let ()  = box#append_child main in
  let ()  = box#append_child (new Divider.t ()) in
  let ()  = box#append_child (make_list_title "Информация из SDT") in
  let ()  = box#append_child sdt in
  let ()  = box#add_class _class in
  box, fun (x:service_info) ->
       set_main x;
       set_sdt x

let make_details (init:service_info list)
      (event:service_info list React.event) =
  let menu_list = make_menu_list () in
  let info, set_info = make_info () in
  object
    inherit Hbox.t ~widgets:[ menu_list#widget; info#widget ] ()
    method menu_list  = menu_list
    method set_info x = set_info x
  end

let get_service_bitrate
      (br:(int * int) list)
      (s:service_info) =
  let ecm = List.fold_left (fun acc (x:ecm_info) ->
                match List.Assoc.get ~eq:(=) x.pid br with
                | None   -> acc
                | Some x -> x + acc) 0 s.ecm in
  let es  = List.fold_left (fun acc (x:es_info) ->
                match List.Assoc.get ~eq:(=) x.pid br with
                | None   -> acc
                | Some x -> x + acc) 0 s.es in
  let pmt = Option.get_or ~default:0
            @@ List.Assoc.get ~eq:(=) s.pmt_pid br in
  ecm + es + pmt

let make_table (init:service_info list)
      (event:service_info list React.event)
      (bitrate:bitrate React.event) =
  let is_hex     = false in
  let hex_id_fmt = Some (Printf.sprintf "0x%04X") in
  let br_fmt     = Table.(Option (Float None, "-")) in
  let pct_fmt    = Table.(Option (Float (Some (Printf.sprintf "%.2f")), "-")) in
  let fmt =
    let open Table in
    let open Format in
    (   to_column "ID", if is_hex then Int hex_id_fmt else Int None)
    :: (to_column "Сервис", String None)
    :: (to_column "PMT PID", if is_hex then Int hex_id_fmt else Int None)
    :: (to_column "PCR PID", if is_hex then Int hex_id_fmt else Int None)
    :: (to_column "Битрейт, Мбит/с", br_fmt)
    :: (to_column "%", pct_fmt)
    :: (to_column "Min, Мбит/с", br_fmt)
    :: (to_column "Max, Мбит/с", br_fmt)
    :: [] in
  let table   = new Table.t ~dense:true ~fmt () in
  (* let tab_bar =
   *   new Tabs.Scroller.t () in *)
  let details = make_details init event in
  let on_change = fun (x:bool) ->
    List.iter (fun row ->
        let open Table in
        match row#cells with
        | id :: _ :: pmt :: pcr :: _ ->
           let fmt = if x then Int hex_id_fmt else Int None in
           id#set_format  fmt;
           pmt#set_format fmt;
           pcr#set_format fmt)
      table#rows in
  let back_ico = new Icon.Button.Font.t ~icon:"arrow_back" () in
  let back_txt = new Typography.Text.t ~adjust_margin:false
                   ~font:Caption
                   ~text:"Назад" () in
  let back    = new Hbox.t
                  ~valign:`Center
                  ~widgets:[ back_ico#widget; back_txt#widget ] () in
  let ()      = back#add_class @@ Markup.CSS.add_element base_class "back" in
  let switch  = new Switch.t ~state:is_hex ~on_change () in
  let hex     = new Form_field.t ~input:switch ~label:"HEX IDs" () in
  let actions = new Card.Actions.t ~widgets:[ back#widget; hex#widget ] () in
  let media   = new Card.Media.t ~widgets:[ table ] () in
  let card    = new Card.t ~widgets:[ actions#widget
                                    ; (new Divider.t ())#widget
                                    ; media#widget ] () in
  let hide w = w#style##.visibility := Js.string "hidden" in
  let show w = w#style##.visibility := Js.string "" in
  back#listen Widget.Event.click (fun _ _ ->
      media#append_child table;
      media#remove_child details;
      show hex; hide back;
      true) |> ignore;
  hide back;
  let add_row (x:service_info) =
    let row = table#add_row (
                  x.id :: x.name :: x.pmt_pid :: x.pcr_pid
                  :: None :: None :: None :: None :: []) in
    row#listen Widget.Event.click (fun _ _ ->
        show back; hide hex;
        media#remove_child table;
        media#append_child details;
        details#set_info x;
        (* (match List.find_opt (fun i -> equal_service_info i#value x)
         *          details#list#items with
         *  | Some item -> details#list#set_active item;
         *                 item#root##scrollIntoView Js._true;
         *  | None      -> ()); *)
        true) |> ignore in
  let _ =
    React.E.map (fun (bitrate:bitrate) ->
        List.iter (fun row ->
            let open Table in
            let id, cur, per, min, max =
              match row#cells with
              | id :: _ :: _ :: _ :: a :: b :: c :: d :: _ ->
                 id, a, b, c, d in
            let br = get_service_bitrate bitrate.pids
                       (List.find (fun (x:service_info) ->
                            x.id = id#value) init) in
            let pct = 100. *. (float_of_int br)
                      /. (float_of_int bitrate.total) in
            let br = (float_of_int br) /. 1_000_000. in
            cur#set_value @@ Some br;
            per#set_value @@ Some pct;
            (match min#value with
             | None -> min#set_value (Some br)
             | Some v -> if br <. v then min#set_value (Some br));
            (match max#value with
             | None -> max#set_value (Some br)
             | Some v -> if br >. v then max#set_value (Some br)))
          table#rows;
        bitrate) bitrate in
  List.iter Fun.(ignore % add_row) init;
  let () = card#add_class base_class in
  card

let make ~(config:config) control =
  let id   = match config.stream.id with
    | `Ts id -> id
    | `Ip _  -> failwith "UDP" in
  let init =
    Requests.Streams.HTTP.get_services ~id ~limit:1 control
    >>= (function
         | Raw s -> Lwt_result.return s.data
         | _     -> Lwt.fail_with "got compressed") in
  let loader =
    init
    >|= (fun init ->
      let e_br, br_sock = Requests.Streams.WS.get_bitrate ~id control in
      let services = match List.head_opt init with
        | Some (_, services) -> services.services
        | None -> [] in
      make_table services React.E.never e_br)
    >|= Widget.coerce
    |> Lwt_result.map_err Api_js.Requests.err_to_string
    |> Ui_templates.Loader.create_widget_loader
  in loader


