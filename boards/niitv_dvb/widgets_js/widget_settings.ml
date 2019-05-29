open Js_of_ocaml
open Application_types
open Board_niitv_dvb_types.Device
open Components
open Widget_types

let name = "Настройки"
let settings = None

let base_class = "dvb-niit-settings"
let body_class = Components_tyxml.BEM.add_element base_class "body"

type event =
  [ `Mode of (int * mode) list
  | `State of Topology.state
  ]

(*

  match receivers with
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
    make ~config:{id} ~state ~config (Some {id}) control
*)

let make_inner state mode receivers control =
  let tabs =
    List.map (fun id ->
        let open Widget_module_settings in
        let name = Printf.sprintf "%s %d" module_name (succ id) in
        let value = make {id} state mode control in
        value#widget, Tab.make ~label:name ())
    @@ List.sort compare receivers in
  let bar, body = Ui_templates.Tabs.create_simple tabs in
  body#add_class body_class;
  Ui_templates.Tabs.wrap bar body

class t mode state receivers control =
  object
    inherit Widget.t (Dom_html.createDiv Dom_html.document) () as super

    val mutable inner = match receivers with
      | None -> None
      | Some x -> Some (make_inner state mode x control)

    method! init () : unit =
      super#init ();
      super#add_class base_class

    method notify : event -> unit = function _ -> ()

  end

let make mode state receivers control =
  new t mode state receivers control
