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

let req_of_table table_id table_id_ext (eit_params:eit_params) section =
  let r =
    Requests.Streams.HTTP.get_si_psi_section
      ~table_id ~section:section in
  match Mpeg_ts.table_of_int table_id with
  | `PAT   -> r ~table_id_ext ?eit_ts_id:None ?eit_orig_nw_id:None
  | `PMT   -> r ~table_id_ext ?eit_ts_id:None ?eit_orig_nw_id:None
  | `NIT _ -> r ~table_id_ext ?eit_ts_id:None ?eit_orig_nw_id:None
  | `SDT _ -> r ~table_id_ext ?eit_ts_id:None ?eit_orig_nw_id:None
  | `BAT   -> r ~table_id_ext ?eit_ts_id:None ?eit_orig_nw_id:None
  | `EIT _ -> r ~table_id_ext
                ~eit_ts_id:eit_params.ts_id
                ~eit_orig_nw_id:eit_params.orig_nw_id
  | _      -> r ?table_id_ext:None ?eit_ts_id:None ?eit_orig_nw_id:None

module Section = struct

  module Id = Int

  type model  = section_info [@@deriving eq]
  type widget = (section_info * section option) Item_list.Item.t

  let widget      = fun w -> w#widget
  let id_of_model = fun (x:model) -> x.id

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
                 ~meta:bytes#widget
                 ~value:(init, None) () in
    let to_primary = Printf.sprintf "ID: %d" in
    let update_primary =
      { get = (fun (x:model) -> x.id)
      ; eq  = (=)
      ; upd = (fun id ->
        let s = to_primary id in
        leaf#set_text s)
      } in
    let update = fun ?(previous:model option) (model:model) ->
      leaf#set_value (model, None);
      setter ?previous model update_primary;
      setter ?previous model update_bytes in
    update init;
    leaf, fun x -> update ~previous:!prev x

end

module Sections =
  Make_array(struct
      module Node = Section

      type widget = (section_info * section option) Item_list.t

      let root (w:widget) = w#root
      let append_child (w:widget) (i:Node.widget) =
        w#append_item i
      let insert_child_at_idx (w:widget) idx (i:Node.widget) =
        i#listen Widget.Event.click (fun _ _ -> w#set_active i; true)
        |> ignore;
        w#insert_item_at_idx idx i
      let remove_child (w:widget) (i:Node.widget) =
        w#remove_item i
      let make (nodes:Node.model list) =
        let items  =
          List.map (fun x -> let i, _ = Node.make x in
                             `Item i) nodes in
        let list   = new Item_list.t
                       ~selection:`Single
                       ~items () in
        List.iter (fun (i:'a Item_list.Item.t) ->
            i#listen Widget.Event.click (fun _ _ -> list#set_active i; true)
            |> ignore) list#items;
        let () = list#set_dense true in
        list, (fun _ -> ())
    end)

let make_list (init:section_info list)
      (event: section_info list React.event)
      (sections: (section_info * section) list)
      control =
  let event : (section_info list * section_info list) React.event =
    React.S.diff (fun n o -> o, n)
    @@ React.S.hold ~eq:(Equal.list Section.equal_model) init event in
  let list, update_list = Sections.make init in
  let () = List.iter (fun x ->
               let i, _ = x#value in
               match List.Assoc.get ~eq:equal_section_info i sections with
               | Some v -> x#set_value (i, Some v)
               | None   -> ()) list#items in
  let _e =
    React.E.map (fun ((prev:section_info list),
                      (model:section_info list)) ->
        if not @@ (Equal.list Section.equal_model) prev model
        then update_list model)
      event in
  let () = list#set_dense true in
  let () = list#set_on_destroy
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
  let header_class   = Markup.CSS.add_element base_class "header" in
  let title_class    = Markup.CSS.add_element base_class "title" in
  let subtitle_class = Markup.CSS.add_element base_class "subtitle" in
  (* Elements *)
  let title     = new Typography.Text.t
                    ~adjust_margin:false
                    ~text:"Выберите секцию для захвата" () in
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
  let () = subtitle#add_class subtitle_class in
  let () = header#add_class header_class in
  header#widget, title, subtitle, button

let make_dump
      (stream:Stream.id)
      (table:table_info)
      (list:(section_info * section option) Item_list.t) control =
  let base_class = Markup.CSS.add_element base_class "dump" in
  let header, title, subtitle, button = make_dump_header base_class () in
  let hexdump, set_hexdump = make_hexdump () in
  let parsed  = make_parsed () in
  let options = make_hexdump_options hexdump in
  let () =
    React.S.map (function
        | Some item ->
           let { id; id_ext; eit_params; _ } = table in
           let (section:section_info), prev_dump = item#value in
           let open Lwt.Infix in
           let text  = Dom_html.createPre Dom_html.document
                       |> Widget.create in
           let err x = Ui_templates.Placeholder.create_with_error ~text:x () in
           let ph  x = Ui_templates.Placeholder.create_with_icon
                         ~icon:"info"
                         ~text:x () in
           let tz_offset_s = Ptime_clock.current_tz_offset_s () in
           let fmt_time = Time.to_human_string ?tz_offset_s in
           let upd = function
             | Some { timestamp; section; parsed = Some x; _ } ->
                parsed#set_empty ();
                subtitle#set_text @@ fmt_time timestamp;
                text#set_text_content (Yojson.Safe.pretty_to_string x);
                Dom.appendChild parsed#root text#root;
                set_hexdump @@ String.of_list @@ List.map Char.chr section
             | Some { timestamp; section; parsed = None; _ } ->
                parsed#set_empty ();
                subtitle#set_text @@ fmt_time timestamp;
                Dom.appendChild parsed#root
                  (ph "Не удалось разобрать содержимое секции")#root;
                set_hexdump @@ String.of_list @@ List.map Char.chr section
             | None     ->
                parsed#set_empty ();
                subtitle#set_text "-";
                Dom.appendChild parsed#root (ph "Нет захваченных данных")#root;
                set_hexdump "" in
           let get = fun () ->
             Lwt.catch (fun () ->
                 (req_of_table id id_ext eit_params section.id)
                   ~id:stream control
                 |> Lwt_result.map_err Api_js.Requests.err_to_string
                 >|= (function
                      | Ok dump ->
                         item#set_value (section, Some dump);
                         parsed#set_empty ();
                         upd (Some dump);
                      | Error s ->
                         parsed#set_empty ();
                         Dom.appendChild parsed#root (err s)#root))
               (fun e ->
                 parsed#set_empty ();
                 Dom.appendChild parsed#root (err @@ Printexc.to_string e)#root;
                 Lwt.return_unit) in
           upd prev_dump;
           let () = button#set_getter (Some get) in
           let () = title#set_text @@ Printf.sprintf "Секция %d" section.id in
           let () = button#set_disabled false in
           ()
        | _ -> ()) list#s_active
    |> Lwt_react.S.keep in
  let vsplit = new Vsplit.t parsed hexdump () in
  object(self)
    inherit Vbox.t ~widgets:[ header
                            ; vsplit#widget
                            ; (new Divider.t ())#widget
                            ; options#widget ] ()

    method button = button

    initializer
      self#add_class base_class
  end

class t ~(config:config)
        ~(init:table_info)
        ~(sections:(section_info * section) list)
        ~(event:table_info React.event)
        (control:int)
        () =
  let stream_panel_class = Markup.CSS.add_element base_class "list" in
  let id  = match config.stream.id with
    | `Ts id -> id
    | `Ip _  -> failwith "UDP" in
  let box   = Widget.create_div () in
  let event = React.E.map (fun x -> x.sections) event in
  let list  = make_list init.sections event sections control in
  let dump  = make_dump id init list control in
  let list_name =
    let _class = Markup.CSS.add_element stream_panel_class "title" in
    let w  = new Typography.Text.t ~text:"Секции" () in
    let () = w#add_class _class in
    w in
  let list_box =
    let box = Widget.create_div () in
    let ()  = box#append_child list_name in
    let ()  = box#append_child list in
    box in
  object(self)
    inherit Hbox.t ~widgets:[ box#widget; dump#widget ] ()

    method list = list
    method dump = dump

    initializer
      box#append_child list_box;
      box#add_class stream_panel_class;
      self#add_class base_class
  end
