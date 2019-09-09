open Js_of_ocaml
open Application_types
open Board_niitv_dvb4ch_types
open Components

let name = "Настройки"

let settings = None

let base_class = "dvb-niit-settings"

let body_class = Components_tyxml.BEM.add_element base_class "body"

type event =
  [ `Mode of (int * Device.mode) list
  | `State of Topology.state
  | `PLPs of (int * Plp_list.t ts) list ]

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

let make_inner state mode plps receivers control =
  let tabs =
    List.map (fun id ->
        let open Module_settings in
        let name = Printf.sprintf "%s %d" Util.module_name (succ id) in
        let mode = List.assoc_opt id mode in
        let plps =
          match List.assoc_opt id plps with
          | None -> []
          | Some (x : Plp_list.t ts) -> x.data.plps
        in
        ( `Widget (make {id} state mode plps control)
        , Tab.Markup_js.create ~text_label:(`Text name) () ))
    @@ List.sort compare receivers
  in
  let bar, body = Tab_bar.make_bind ~tabs () in
  body#add_class body_class;
  ( List.filter_map
      (function
        | `Widget x, _ -> Some x
        | _ -> None)
      tabs
  , object
      inherit Widget.t Dom_html.(createDiv document) () as super

      method! init () : unit =
        super#append_child bar;
        super#append_child body;
        super#init ()

      method! destroy () : unit =
        bar#destroy ();
        body#destroy ();
        super#destroy ()
    end )

class t state mode plps receivers control =
  let modules, inner =
    match receivers with
    | None -> [], None
    | Some x ->
        let x, y = make_inner state mode plps x control in
        x, Some y
  in
  object
    inherit Widget.t (Dom_html.createDiv Dom_html.document) () as super

    method! init () : unit =
      super#init ();
      super#add_class base_class;
      Option.iter super#append_child inner

    method! destroy () : unit =
      List.iter Widget.destroy modules;
      super#destroy ()

    method notify : event -> unit =
      function
      | `Mode _x -> ()
      | `PLPs x ->
          List.iter
            (fun (id, ({data; _} : Plp_list.t ts)) ->
              match List.find_opt (fun w -> w#id = id) modules with
              | None -> ()
              | Some m -> m#notify (`PLPs data.plps))
            x
      | `State _ -> ()
  end

let make state mode plps receivers control = new t state mode plps receivers control
