open Containers
open Components
open Lwt_result.Infix
open Widget_types

let name = "Настройки"
let settings = None

let base_class = "dvb-niit-settings"
let body_class = Markup.CSS.add_element base_class "body"

let make_inner (parent : #Widget.t) state config control receivers =
  let w = match receivers with
    | None ->
       let ph =
         Ui_templates.Placeholder.create_with_icon
           ~text:"Устройство не готово"
           ~icon:Icon.SVG.(create_simple Path.information)
           () in
       ph#widget
    | Some [] ->
       let ph =
         Ui_templates.Placeholder.create_with_error
           ~text:"Нет доступных тюнеров"
           () in
       ph#widget
    | Some [id] ->
       let open Widget_module_settings in
       make ~state ~config (Some {id}) control
    | Some ids ->
       let tabs =
         List.map (fun id ->
             let open Widget_module_settings in
             let name = Printf.sprintf "%s %d" module_name (succ id) in
             let value = make ~state ~config (Some {id}) control in
             new Tab.t ~value ~content:(Text name) ())
         @@ List.sort compare ids in
       let bar, body = Ui_templates.Tabs.create_simple tabs in
       body#add_class body_class;
       Ui_templates.Tabs.wrap bar body in
  parent#set_empty ();
  parent#append_child w

let make ~(state : Common.Topology.state React.signal)
      ~(config : Board_types.Device.config React.signal)
      ~(receivers : int list option React.signal)
      control =
  let w = Widget.create_div () in
  let _s = React.S.map (make_inner w state config control) receivers in
  w#set_on_destroy @@ Some (fun () -> React.S.stop ~strong:true _s);
  w#add_class base_class;
  w
