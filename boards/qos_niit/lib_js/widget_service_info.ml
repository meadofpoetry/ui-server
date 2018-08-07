open Containers
open Components
open Common
open Board_types.Streams.TS

let base_class = "qos-niit-service-info"

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

let make_description () =
  let _class = Markup.CSS.add_element base_class "description" in
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

let make_pids () =
  (* FIXME implement *)
  Widget.create_div ()

let make () =
  let info, set_info = make_description () in
  let pids = make_pids () in
  let tabs =
    let info_icon =
      Icon.SVG.(create_simple Path.file_document_box_outline) in
    let list_icon =
      Icon.SVG.(create_simple Path.view_list) in
    [ new Tab.t ~content:(Both ("Описание", info_icon)) ~value:info ()
    ; new Tab.t ~content:(Both ("PIDs", list_icon)) ~value:pids () ] in
  let div    = Widget.create_div () in
  let bar, _ = Ui_templates.Tabs.create_simple ~body:div tabs in
  object
    inherit Vbox.t ~widgets:[ bar#widget
                            ; (new Divider.t ())#widget
                            ; div ] ()
    method set_info x = set_info x
  end
