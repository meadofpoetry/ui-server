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

let make_back () =
  let back_ico = new Icon.Button.Font.t ~icon:"arrow_back" () in
  let back_txt =
    new Typography.Text.t
      ~adjust_margin:false
      ~font:Caption
      ~text:"Назад" () in
  let back =
    new Hbox.t
      ~valign:`Center
      ~widgets:[ back_ico#widget; back_txt#widget ] () in
  back#add_class @@ Markup.CSS.add_element base_class "back";
  back

let make_table
      (init:service_info list)
      (event:service_info list React.event) =
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
  let table     = new Table.t ~dense:true ~fmt () in
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
  let switch  = new Switch.t ~state:is_hex ~on_change () in
  table, switch

let map_details (details:Widget_service_info.t option ref)
      (info:service_info) =
  match !details with
  | None   -> None
  | Some x ->
     if equal_service_info x#info info
     then Some x
     else None

let set_bitrate init bitrate details' row =
  let open Table in
  let id, cur, per, min, max =
    match row#cells with
    | id :: _ :: _ :: _ :: a :: b :: c :: d :: _ ->
       id, a, b, c, d in
  match List.find_opt (fun (x:service_info) ->
            x.id = id#value) init with
  | Some info ->
     let br, pct = Widget_service_info.map_bitrate info bitrate in
     let details = map_details details' info in
     cur#set_value @@ Some br;
     per#set_value @@ Some pct;
     Option.iter (fun x -> x#set_rate @@ Some br) details;
     (match min#value with
      | None   ->
         min#set_value @@ Some br;
         Option.iter (fun x -> x#set_min @@ Some br) details;
      | Some v ->
         if br <. v
         then (min#set_value @@ Some br;
               Option.iter (fun x -> x#set_min @@ Some br) details));
     (match max#value with
      | None   ->
         max#set_value @@ Some br;
         Option.iter (fun x -> x#set_max @@ Some br) details;
      | Some v ->
         if br >. v
         then (max#set_value @@ Some br;
               Option.iter (fun x -> x#set_max @@ Some br) details))
  | None      -> ()

let make_card (init:service_info list)
      (event:service_info list React.event)
      (bitrate:bitrate React.event) =
  let table, switch = make_table init event in
  let hex     = new Form_field.t ~input:switch ~label:"HEX IDs" () in
  let actions = new Card.Actions.t ~widgets:[ hex#widget ] () in
  let media   = new Card.Media.t ~widgets:[ table ] () in
  let card    =
    new Card.t ~widgets:[ actions#widget
                        ; (new Divider.t ())#widget
                        ; media#widget ] () in
  let hide w = w#style##.visibility := Js.string "hidden" in
  let show w = w#style##.visibility := Js.string "" in
  let details' = ref None in
  let add_row (x:service_info) =
    let row =
      table#add_row (x.id :: x.name :: x.pmt_pid :: x.pcr_pid
                     :: None :: None :: None :: None :: []) in
    row#listen_lwt Widget.Event.click (fun _ _ ->
        let open Lwt.Infix in
        let cur, min, max =
          let open Table in
          match row#cells with
          | _ :: _ :: _ :: _ :: a :: _ :: b :: c :: _ -> a, b, c in
        let back    = make_back () in
        let details = Widget_service_info.make x in
        details#set_rate cur#value;
        details#set_min min#value;
        details#set_max max#value;
        details' := Some details;
        back#listen_once_lwt Widget.Event.click
        >|= (fun _ -> media#append_child table;
                      media#remove_child details;
                      actions#remove_child back;
                      show hex)
        |> Lwt.ignore_result;
        actions#insert_child_at_idx 0 back;
        hide hex;
        media#remove_child table;
        media#append_child details;
        Lwt.return_unit)
    |> Lwt.ignore_result in
  let _ =
    React.E.map (fun (bitrate:bitrate) ->
        (* FIXME change init to actual list *)
        List.iter (set_bitrate init bitrate details') table#rows;
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
      make_card services React.E.never e_br)
    >|= Widget.coerce
    |> Lwt_result.map_err Api_js.Requests.err_to_string
    |> Ui_templates.Loader.create_widget_loader
  in loader


