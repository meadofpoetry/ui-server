open Containers
open Components
open Board_types
open Board_types.SI_PSI_section
open Ui_templates.Sdom
open Common

let name = "Таблицы"

let settings = None

let base_class = "qos-niit-tables"

let req_of_table table_id table_id_ext id_ext_1 id_ext_2 section =
  let r = Requests.Streams.HTTP.get_si_psi_section
            ~table_id ~section:section in
  match Mpeg_ts.table_of_int table_id with
  | `PAT -> r ~table_id_ext ?id_ext_1:None ?id_ext_2:None
  | `PMT -> r ~table_id_ext ?id_ext_1:None ?id_ext_2:None
  | `NIT _ -> r ~table_id_ext ?id_ext_1:None ?id_ext_2:None
  | `SDT _ -> r ~table_id_ext ~id_ext_1 ?id_ext_2:None
  | `BAT -> r ~table_id_ext ?id_ext_1:None ?id_ext_2:None
  | `EIT _ -> r ~table_id_ext ~id_ext_1 ~id_ext_2
  | _ -> r ?table_id_ext:None ?id_ext_1:None ?id_ext_2:None

let dump_error_to_string : Dump.error -> string = function
  | Zero_length -> "Пустая секция"
  | Stream_not_found -> "Поток не найден"
  | Table_not_found -> "Таблица не найдена"
  | Section_not_found -> "Секция не найдена"
  | Unknown -> "Неизвестная ошибка"

module Section = struct

  open SI_PSI_table

  module Id = Int

  type model  = section_info * Dump.t Time.timestamped option
  type widget = (section_info * Dump.t Time.timestamped option) Item_list.Item.t

  let equal_model (a : model) (b : model) =
    equal_section_info (fst a) (fst b)

  let widget = fun w -> w#widget
  let id_of_model = fun ((x : section_info), _) -> x.section

  let make (init : model) =
    let bytes, update_bytes =
      let to_string x =
        let s = match x mod 10 with
          | 2 | 3 | 4 when x / 10 <> 1 -> "байта"
          | _ -> "байт" in
        Printf.sprintf "%d %s" x s in
      let w = new Typography.Text.t ~text:"" () in
      let v = { get = (fun (x : model) -> (fst x).length)
              ; eq  = Int.equal
              ; upd = Fun.(w#set_text % to_string) } in
      w, v in
    let prev = ref init in
    let leaf = new Item_list.Item.t
                 ~text:""
                 ~meta:bytes#widget
                 ~value:init () in
    let to_primary = Printf.sprintf "ID: %d" in
    let update_primary =
      { get = (fun (x : model) -> (fst x).section)
      ; eq = ( = )
      ; upd = (fun id ->
        let s = to_primary id in
        leaf#set_text s)
      } in
    let update = fun ?(previous : model option) (model : model) ->
      leaf#set_value model;
      setter ?previous model update_primary;
      setter ?previous model update_bytes in
    update init;
    leaf, update ~previous:!prev

end

module Sections =
  Make_array(struct
      open SI_PSI_table

      module Node = Section

      type widget = (section_info * Dump.t Time.timestamped option) Item_list.t

      let root (w : widget) = w#root

      let append_child (w : widget) (i : Node.widget) =
        i#listen Widget.Event.click (fun _ _ -> w#set_active i; true)
        |> ignore;
        w#append_item i

      let insert_child_at_idx (w : widget) idx (i : Node.widget) =
        i#listen Widget.Event.click (fun _ _ -> w#set_active i; true)
        |> ignore;
        w#insert_item_at_idx idx i

      let remove_child (w : widget) (i : Node.widget) =
        w#remove_item i

      let make (_ : Node.model list) =
        let list = new Item_list.t
                     ~dense:true
                     ~selection:`Single
                     ~items:[] () in
        list, (fun _ -> ())
    end)

let make_list
      (sections : (SI_PSI_table.section_info * Dump.t Time.timestamped option) list) =
  let list, update_list = Sections.make sections in
  list#set_dense true;
  list, update_list

let make_parsed () =
  let base_class = CSS.add_element base_class "parsed" in
  let body = Widget.create_div () in
  body#add_class base_class;
  body

let make_hexdump_options hexdump =
  let base_class = CSS.add_element base_class "hexdump-options" in
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
      ~items:[ `Item (new Select.Item.t ~value:4 ~text:"4"  ())
             ; `Item (new Select.Item.t ~value:8 ~text:"8" ~selected:true ())
             ; `Item (new Select.Item.t ~value:16 ~text:"16" ())
             ; `Item (new Select.Item.t ~value:32 ~text:"32" ()) ]
      () in
  let line_numbers =
    new Form_field.t
      ~input:(new Switch.t ~state:true ())
      ~label:"Номера"
      () in
  let options =
    new Hbox.t
      ~widgets:[ base#widget
               ; width#widget
               ; line_numbers#widget ]
      () in
  let s_line_num =
    React.S.map ~eq:Equal.unit
      hexdump#set_line_numbers
      line_numbers#input_widget#s_state in
  let s_width =
    React.S.map ~eq:Equal.unit (function
        | None -> ()
        | Some x -> hexdump#set_width x)
      width#s_selected_value in
  let s_base =
    React.S.map ~eq:Equal.unit (function
        | None -> ()
        | Some x -> hexdump#set_base x)
      base#s_selected_value in
  options#add_class base_class;
  options#set_on_destroy (fun () ->
      React.S.stop ~strong:true s_line_num;
      React.S.stop ~strong:true s_width;
      React.S.stop ~strong:true s_base);
  options#widget

let make_hexdump () =
  let config = Hexdump.make_config ~width:16 () in
  let hexdump = new Hexdump.t ~interactive:false ~config "" () in
  hexdump, hexdump#set_bytes

let make_dump_header base_class () =
  (* CSS classes *)
  let header_class = CSS.add_element base_class "header" in
  let title_class = CSS.add_element base_class "title" in
  let subtitle_class = CSS.add_element base_class "subtitle" in
  (* Elements *)
  let title =
    new Typography.Text.t
      ~adjust_margin:false
      ~text:"Выберите секцию для захвата"
      () in
  let subtitle =
    new Typography.Text.t
      ~adjust_margin:false
      ~split:true
      ~text:""
      () in
  let button =
    new Ui_templates.Buttons.Get.t
      ~style:`Raised
      ~label:"Загрузить"
      () in
  let title_box =
    new Vbox.t
      ~widgets:[ title#widget
               ; subtitle#widget ]
      () in
  let header =
    new Hbox.t
      ~halign:`Space_between
      ~widgets:[ title_box#widget
               ; button#widget ]
      () in
  (* CSS classes setup *)
  title#add_class title_class;
  subtitle#add_class subtitle_class;
  header#add_class header_class;
  header#widget, title, subtitle, button

let integer_to_dec : Dump.integer -> string = function
  | Bool x -> if x then "1" else "0"
  | Int x -> string_of_int x
  | Int32 x -> Int32.to_string x
  | Int64 x -> Int64.to_string x
  | Uint x -> Printf.sprintf "%u" x
  | Uint32 x -> Printf.sprintf "%lu" x
  | Uint64 x -> Printf.sprintf "%Lu" x

let integer_to_hex : Dump.integer -> string = function
  | Bool x -> if x then "1" else "0"
  | Int x -> Printf.sprintf "0x%X" x
  | Int32 x -> Printf.sprintf "0x%lX" x
  | Int64 x -> Printf.sprintf "0x%LX" x
  | Uint x -> Printf.sprintf "%0xX" x
  | Uint32 x -> Printf.sprintf "0x%lX" x
  | Uint64 x -> Printf.sprintf "0x%LX" x

let integer_to_bits : Dump.integer -> string = function
  | Bool x -> if x then "1" else "0"
  | Int x -> Printf.sprintf "0x%X" x
  | Int32 x -> Printf.sprintf "0x%lX" x
  | Int64 x -> Printf.sprintf "0x%LX" x
  | Uint x -> Printf.sprintf "%0xX" x
  | Uint32 x -> Printf.sprintf "0x%lX" x
  | Uint64 x -> Printf.sprintf "0x%LX" x

let make_tree (x : Dump.parsed) =
  let open Dump in
  let value_to_string : value -> string = function
    | Bytes s -> String.concat " "
                 @@ List.map Fun.(String.of_char % Char.chr) s
    | Bits x -> integer_to_bits x
    | Dec x -> integer_to_dec x
    | Hex x -> integer_to_hex x
    | String s -> s
    | Time t-> let tz_offset_s = Ptime_clock.current_tz_offset_s () in
               Format.asprintf "%a" (Time.pp_human ?tz_offset_s ()) t
    | Duration d ->
       let w, d, h, m, s = Time.Relative.split_units d in
       List.filter_map (fun v ->
           let v, unit = v in
           if v = 0 then None else Some (string_of_int v ^ " " ^ unit))
         [ w, "нед"; d, "дн"; h, "ч"; m, "мин"; s, "сек" ]
       |> String.concat " "
    | List _ -> "" in
  let rec aux acc = function
    | [] -> List.rev acc
    | hd :: tl ->
       let value, name = hd.value in
       let vs = value_to_string value in
       let text = match name with
         | None -> vs
         | Some n -> Printf.sprintf "%s (%s)" n vs in
       let meta, nested = match value with
         | List [] ->
            let meta = Icon.SVG.(create_simple Path.code_brackets) in
            Some meta#widget, None
         | List l ->
            let items = aux [] l in
            let tree  = new Tree.t ~items () in
            None, Some tree
         | _ -> Some (new Typography.Text.t ~text ())#widget, None in
       let item = new Tree.Item.t ?meta ?nested ~value:hd ~text:hd.name () in
       let info = Printf.sprintf "Длина: %d, смещение: %d" hd.length hd.offset in
       item#set_attribute "title" info;
       aux (item :: acc) tl in
  let items = aux [] x in
  let tree  = new Tree.t ~dense:true ~items () in
  tree

let make_dump
      ~(table_id : int)
      ~(table_id_ext : int)
      ~(id_ext_1 : int)
      ~(id_ext_2 : int)
      (stream : Stream.ID.t)
      (list : (SI_PSI_table.section_info * Dump.t Time.timestamped option) Item_list.t)
      (control : int) =
  let open SI_PSI_table in
  let base_class = CSS.add_element base_class "dump" in
  let header, title, subtitle, button = make_dump_header base_class () in
  let hexdump, set_hexdump = make_hexdump () in
  let parsed = make_parsed () in
  let options = make_hexdump_options hexdump in
  let vsplit = new Vsplit.t parsed hexdump () in
  object(self)
    inherit Vbox.t
              ~widgets:[ header
                       ; vsplit#widget
                       ; (new Divider.t ())#widget
                       ; options#widget ]
              () as super

    method! init () : unit =
      super#init ();
      self#add_class base_class;
      React.S.map ~eq:(fun _ _ -> false) self#on_active_change list#s_active
      |> self#_keep_s

    method button = button

    method on_active_change = function
      | None -> ()
      | Some item ->
         let (section : section_info), prev_dump = item#value in
         let open Lwt.Infix in
         let open Dump in
         let err x = Ui_templates.Placeholder.create_with_error ~text:x () in
         let ph x =
           Ui_templates.Placeholder.create_with_icon
             ~icon:Icon.SVG.(create_simple Path.information)
             ~text:x () in
         let tz_offset_s = Ptime_clock.current_tz_offset_s () in
         let fmt_time = Time.to_human_string ?tz_offset_s in
         let upd : Dump.t Time.timestamped option -> unit = function
           | Some { timestamp; data = { section; content = Some x; _ } } ->
              parsed#set_empty ();
              subtitle#set_text @@ fmt_time timestamp;
              let tree = make_tree x in
              let rec get_items acc = function
                | [] -> acc
                | hd :: tl ->
                   let acc = match hd#nested_tree with
                     | None      ->
                        (match hd#value.value with
                         | List _, _ -> acc
                         | _ -> hd :: acc)
                     | Some tree -> get_items acc tree#items in
                   get_items acc tl in
              let items = get_items [] tree#items in
              List.iter (fun (i : (node, _) Tree.Item.t) ->
                  i#listen_lwt Widget.Event.click (fun _ _ ->
                      let { offset; length; _ } = i#value in
                      let res, from = offset mod 8, offset / 8 in
                      let len  = float_of_int @@ length + res in
                      let len  = int_of_float @@ ceil @@ len /. 8. in
                      let till = from + (pred len) in
                      hexdump#select_range from till;
                      tree#set_active i;
                      Lwt.return_unit) |> Lwt.ignore_result) items;
              parsed#append_child tree;
              set_hexdump @@ String.of_list @@ List.map Char.chr section
           | Some { timestamp; data = { section; content = None; _ }} ->
              let ph = ph "Не удалось разобрать содержимое секции" in
              parsed#set_empty ();
              subtitle#set_text @@ fmt_time timestamp;
              parsed#append_child ph;
              set_hexdump @@ String.of_list @@ List.map Char.chr section
           | None ->
              parsed#set_empty ();
              subtitle#set_text "-";
              parsed#append_child (ph "Нет захваченных данных");
              set_hexdump "" in
         let get = fun () ->
           Lwt.catch (fun () ->
               (req_of_table table_id table_id_ext
                  id_ext_1 id_ext_2 section.section)
                 ~id:stream control
               |> Lwt_result.map_err
                    (Api_js.Requests.err_to_string
                       ~to_string:dump_error_to_string)
               >|= (function
                    | Ok dump ->
                       item#set_value (section, Some dump);
                       parsed#set_empty ();
                       upd (Some dump);
                    | Error s ->
                       parsed#set_empty ();
                       parsed#append_child (err s)))
             (fun e ->
               parsed#set_empty ();
               parsed#append_child (err @@ Printexc.to_string e);
               Lwt.return_unit) in
         upd prev_dump;
         button#set_getter (Some get);
         title#set_text @@ Printf.sprintf "Секция %d" section.section;
         button#set_disabled false;
         ()

  end

class t ~(stream : Stream.ID.t)
        ~(table_id : int)
        ~(table_id_ext : int)
        ~(id_ext_1 : int)
        ~(id_ext_2 : int)
        ~(sections : (SI_PSI_table.section_info * Dump.t Time.timestamped option) list)
        (control : int)
        () =
  let stream_panel_class = CSS.add_element base_class "list" in
  let list, update_list = make_list sections in
  let dump = make_dump ~table_id ~table_id_ext
               ~id_ext_1 ~id_ext_2 stream list control in
  let list_name =
    let _class = CSS.add_element stream_panel_class "title" in
    let w = new Typography.Text.t ~text:"Секции" () in
    w#add_class _class;
    w in
  let list_box =
    Widget.create_div ~widgets:[list_name#widget; list#widget] () in
  let box = Widget.create_div ~widgets:[list_box] () in
  object(self)
    inherit Hbox.t ~widgets:[box#widget; dump#widget] () as super

    method! init () : unit =
      super#init ();
      box#add_class stream_panel_class;
      self#add_class base_class

    method! destroy () : unit =
      super#destroy ();
      dump#destroy ();
      list#destroy ();
      box#destroy ();

    method list = list

    method dump = dump

    method set_hex (_ : bool) : unit = ()

    method update (data : SI_PSI_table.section_info list) : unit =
      let eq (a : SI_PSI_table.section_info as 'a) (b : 'a) =
        a.section = b.section in
      let items = List.map (fun x -> x#value) list#items in
      let data =
        List.map (fun x ->
            let dump = Option.flatten @@ List.Assoc.get ~eq x items in
            x, dump) data in
      update_list data
  end
