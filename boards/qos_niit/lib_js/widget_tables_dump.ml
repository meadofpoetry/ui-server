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

let name = "Таблицы"

let settings = None

let base_class = "qos-niit-tables"

let ( % ) = Fun.( % )

let req_of_table (table:table_info) =
  let table_id_ext = table.id_ext in
  let eit_params   = table.eit_params in
  let r =
    Requests.Streams.HTTP.get_si_psi_section
      ~table_id:table.id
      ~section:table.section in
  match Mpeg_ts.table_of_int table.id with
  | `PAT   -> r ~table_id_ext ?eit_ts_id:None ?eit_orig_nw_id:None
  | `PMT   -> r ~table_id_ext ?eit_ts_id:None ?eit_orig_nw_id:None
  | `NIT _ -> r ~table_id_ext ?eit_ts_id:None ?eit_orig_nw_id:None
  | `SDT _ -> r ~table_id_ext ?eit_ts_id:None ?eit_orig_nw_id:None
  | `BAT   -> r ~table_id_ext ?eit_ts_id:None ?eit_orig_nw_id:None
  | `EIT _ -> r ~table_id_ext
                ~eit_ts_id:eit_params.ts_id
                ~eit_orig_nw_id:eit_params.orig_nw_id
  | _      -> r ?table_id_ext:None ?eit_ts_id:None ?eit_orig_nw_id:None

let to_table_name table =
  let divider  = ", " in
  let name     = Mpeg_ts.(table_to_string @@ table_of_int table.id) in
  let id s x   = Printf.sprintf "%s=0x%02X(%d)" s x x in
  let base     = id "table_id" table.id in
  let section  = Printf.sprintf "секция %d" table.section in
  let specific = match Mpeg_ts.table_of_int table.id with
    | `PAT   -> Some [ id "tsid" table.id_ext ]
    | `PMT   -> Some [ id "program" table.id_ext ]
    | `NIT _ -> Some [ id "network_id" table.id_ext ]
    | `SDT _ -> Some [ id "tsid" table.id_ext ]
    | `BAT   -> Some [ id "bid" table.id_ext ]
    | `EIT _ -> Some [ id "sid" table.id_ext
                     ; id "tsid" table.eit_params.ts_id
                     ; id "onid" table.eit_params.orig_nw_id ]
    | _      -> None in
  match specific with
  | Some l -> name, String.concat divider (base :: l @ [ section ])
  | None   -> name, base ^ divider ^ section

module Table = struct

  module Id = struct
    type t     =
      { id        : int
      ; id_ext    : int
      ; section   : int
      ; version   : int
      ; eit_ts_id : int
      ; eit_nw_id : int
      } [@@deriving yojson, ord]
    let to_string x   = to_yojson x |> Yojson.Safe.to_string
    let of_string s = Yojson.Safe.from_string s
                      |> of_yojson
                      |> Result.to_opt
  end

  type model  = table_info
  type widget = (table_info * section option) Item_list.Item.t

  let widget      = fun w -> w#widget
  let equal_model = equal_table_info
  let id_of_model = fun (x:model)  ->
    let open Id in
    { id        = x.id
    ; section   = x.section
    ; version   = x.version
    ; id_ext    = x.id_ext
    ; eit_ts_id = x.eit_params.ts_id
    ; eit_nw_id = x.eit_params.orig_nw_id }

  let make (init:model) =
    let bytes, update_bytes =
      let to_string x =
        let s = if x > 1 && x < 5 then "байта" else "байт" in
        Printf.sprintf "%d %s" x s in
      let w = new Typography.Text.t ~text:"" () in
      let v = { get = (fun (x:model) -> x.length)
              ; eq  = Int.equal
              ; upd = (w#set_text % to_string) } in
      w, v in
    let prev = ref init in
    let leaf = new Item_list.Item.t
                 ~text:""
                 ~secondary_text:""
                 ~meta:bytes#widget
                 ~value:(init, None) () in
    let to_primary   = Printf.sprintf "%s" in
    let to_secondary = Printf.sprintf "id: %d, section: %d, PID: %d, ver: %d" in
    let update_primary =
      { get = (fun x -> Mpeg_ts.(table_to_string @@ table_of_int x.id))
      ; eq  = String.equal
      ; upd = (fun name ->
        let s = to_primary name in
        leaf#set_text s)
      } in
    let update_secondary =
      { get = (fun (x:table_info) -> x.id, x.section, x.pid, x.version)
      ; eq  = (fun (id1, sec1, pid1, ver1)
                   (id2, sec2, pid2, ver2) ->
        id1 = id2 && pid1 = pid2 && ver1 = ver2 && sec1 = sec2)
      ; upd = (fun (id, sec, pid, ver) ->
        let s = to_secondary id sec pid ver in
        leaf#set_secondary_text s)
      } in
    print_endline "make table!";
    let update = fun ?(previous:model option) (model:model) ->
      leaf#set_value (model, None);
      setter ?previous model update_primary;
      setter ?previous model update_secondary;
      setter ?previous model update_bytes in
    update init;
    leaf, fun x -> update ~previous:!prev x

end

module Tables =
  Make_array(struct
      module Node = Table

      type widget = (table_info * section option) Item_list.t

      let root (w:widget) = w#root
      let append_child (w:widget) (i:Node.widget) =
        i#listen Widget.Event.click (fun _ _ -> w#set_active i; true)
        |> ignore;
        w#append_item i
      let insert_child_at_idx (w:widget) idx (i:Node.widget) =
        i#listen Widget.Event.click (fun _ _ -> w#set_active i; true)
        |> ignore;
        w#insert_item_at_idx idx i
      let remove_child (w:widget) (i:Node.widget) =
        w#remove_item i
      let make (nodes:Node.model list) =
        let items  = List.map (fun x -> let w, _ = Node.make x in
                                        `Item w) nodes in
        let list   = new Item_list.t
                       ~two_line:true
                       ~selection:`Single
                       ~items () in
        let ()     = list#set_dense true in
        list, (fun _ -> ())
    end)

let make_stream (init:  table_info list)
      (event: table_info list React.event)
      control =
  let event : (table_info list * table_info list) React.event =
    React.S.diff (fun n o -> o, n)
    @@ React.S.hold ~eq:(Equal.list equal_table_info) init event in
  let list, update_list = Tables.make init in
  let _e =
    React.E.map (fun ((prev:table_info list),
                      (model:table_info list)) ->
        if not @@ (Equal.list equal_table_info) prev model
        then update_list model)
      event in
  let ()   = list#set_dense true in
  let ()   = list#set_on_destroy
             @@ Some (fun () -> React.E.stop ~strong:true _e) in
  list

let make_parsed () =
  let base_class = Markup.CSS.add_element base_class "parsed" in
  let body       = Dom_html.createDiv Dom_html.document |> Widget.create in
  let ()         = body#add_class base_class in
  body#widget

let make_hexdump_options hexdump =
  let base_class = Markup.CSS.add_element base_class "hexdump-options" in
  let base =
    new Select.t
      ~label:"Основание"
      ~items:[ `Item (new Select.Item.t ~value:`Hex ~text:"16" ())
             ; `Item (new Select.Item.t ~value:`Dec ~text:"10" ())
             ; `Item (new Select.Item.t ~value:`Bin ~text:"2" ()) ]
      () in
  let width =
    new Select.t
      ~label:"Ширина"
      ~items:[ `Item (new Select.Item.t ~value:4  ~text:"4"  ())
             ; `Item (new Select.Item.t ~value:8  ~text:"8" ~selected:true ())
             ; `Item (new Select.Item.t ~value:16 ~text:"16" ())
             ; `Item (new Select.Item.t ~value:32 ~text:"32" ()) ]
      () in
  let line_numbers  = new Switch.t ~state:true () in
  let line_numbers' = new Form_field.t ~input:line_numbers
                        ~label:"Номера" () in
  let options = new Hbox.t
                  ~widgets:[ base#widget
                           ; width#widget
                           ; line_numbers'#widget ] () in
  let () = options#add_class base_class in
  let _  = React.S.map hexdump#set_line_numbers line_numbers#s_state in
  let _  = React.S.map (function
               | Some x -> hexdump#set_width x
               | None   -> ()) width#s_selected_value in
  let _  = React.S.map (function
               | Some x -> hexdump#set_base x
               | None   -> ()) base#s_selected_value in
  options#widget

let make_hexdump () =
  let config  = Hexdump.to_config ~width:16 () in
  let hexdump = new Hexdump.t ~config "" () in
  hexdump, hexdump#set_bytes

let make_dump_header base_class () =
  (* CSS classes *)
  let header_class = Markup.CSS.add_element base_class "header" in
  let title_class  = Markup.CSS.add_element base_class "title" in
  (* Elements *)
  let title     = new Typography.Text.t
                    ~adjust_margin:false
                    ~text:"Выберите секцию таблицы SI/PSI для захвата" () in
  let subtitle  = new Typography.Text.t
                    ~adjust_margin:false
                    ~split:true
                    ~text:"" () in
  let button    = new Ui_templates.Buttons.Get.t
                    ~style:`Raised
                    ~label:"Загрузить" () in
  let title_box = new Vbox.t
                    ~widgets:[ title#widget
                             ; subtitle#widget ] () in
  let header    = new Hbox.t
                    ~halign:`Space_between
                    ~widgets:[ title_box#widget
                             ; button#widget] () in
  (* CSS classes setup *)
  let () = title#add_class title_class in
  let () = header#add_class header_class in
  header#widget, title, subtitle, button

let make_dump (stream:Stream.id)
      (tables:(table_info * section option) Item_list.t) control =
  let base_class = Markup.CSS.add_element base_class "dump" in
  let header, title, subtitle, button = make_dump_header base_class () in
  let hexdump, set_hexdump = make_hexdump () in
  let parsed  = make_parsed () in
  let options = make_hexdump_options hexdump in
  let () =
    React.S.map (function
        | Some item ->
           let open Lwt.Infix in
           let name  = to_table_name @@ fst item#value in
           let text  = Dom_html.createPre Dom_html.document
                       |> Widget.create in
           let err x = Ui_templates.Placeholder.create_with_error ~text:x () in
           let ph  x = Ui_templates.Placeholder.create_with_icon
                         ~icon:"info"
                         ~text:x () in
           let upd = function
             | Some { section; parsed = Some x; _ } ->
                parsed#set_empty ();
                text#set_text_content (Yojson.Safe.pretty_to_string x);
                Dom.appendChild parsed#root text#root;
                set_hexdump section
             | Some { section; parsed = None; _ } ->
                parsed#set_empty ();
                Dom.appendChild parsed#root
                  (ph "Не удалось разобрать содержимое секции")#root;
                set_hexdump section
             | None     ->
                parsed#set_empty ();
                Dom.appendChild parsed#root (ph "Нет захваченных данных")#root;
                set_hexdump "" in
           let get = fun () ->
             Lwt.catch (fun () ->
                 (req_of_table @@ fst item#value) ~id:stream control
                 |> Lwt_result.map_err Api_js.Requests.err_to_string
                 >|= (function
                      | Ok dump ->
                         let info, prev = item#value in
                         item#set_value (info, Some dump);
                         parsed#set_empty ();
                         upd (Some dump);
                      | Error s -> parsed#set_empty ();
                                   Dom.appendChild parsed#root (err s)#root))
               (fun e ->
                 parsed#set_empty ();
                 Dom.appendChild parsed#root (err @@ Printexc.to_string e)#root;
                 Lwt.return_unit) in
           upd @@ snd item#value;
           let () = button#set_getter (Some get) in
           let () = title#set_text @@ fst name in
           let () = subtitle#set_text @@ snd name in
           let () = button#set_disabled false in
           ()
        | _ -> ()) tables#s_active
    |> Lwt_react.S.keep in
  let vsplit = new Vsplit.t parsed hexdump () in
  let vbox   = new Vbox.t ~widgets:[ header
                                   ; (new Divider.t ())#widget
                                   ; vsplit#widget
                                   ; (new Divider.t ())#widget
                                   ; options#widget ] () in
  vbox#add_class base_class;
  vbox#widget

class t ~(config:config)
        ~(init:table_info list)
        ~(event:table_info list React.event)
        (control:int)
        () =
  let stream_panel_class = Markup.CSS.add_element base_class "stream-panel" in
  let id  = match config.stream.id with
    | `Ts id -> id
    | `Ip _  -> failwith "UDP" in
  let box = Dom_html.createDiv Dom_html.document
            |> Widget.create in
  let make_struct init =
    let wdg = make_stream init event control in
    wdg in
  let tables = make_struct init in
  let dump = make_dump id tables control in
  object(self)
    inherit Hbox.t ~widgets:[ box; dump ] ()

    method tables = tables

    initializer
      Dom.appendChild box#root tables#root;
      box#add_class stream_panel_class;
      self#add_class base_class
  end
