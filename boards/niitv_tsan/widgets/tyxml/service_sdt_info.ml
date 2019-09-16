open Components_tyxml
open Application_types
open Board_niitv_tsan_types

module CSS = struct
  let root = Util.CSS.root ^ "-service-sdt-info"

  let item = BEM.add_element root "item"
end

let not_available = "n/a"

module Make
    (Xml : Xml_sigs.NoWrap)
    (Svg : Svg_sigs.NoWrap with module Xml := Xml)
    (Html : Html_sigs.NoWrap with module Xml := Xml and module Svg := Svg) =
struct
  open Html
  module Item_list_markup = Item_list.Make (Xml) (Svg) (Html)
  module Icon_markup = Icon.Make (Xml) (Svg) (Html)

  let create_item_meta ?(classes = []) ?(attrs = []) ~text () =
    let classes = Item_list.CSS.item_meta :: classes in
    span ~a:([a_class classes] @ attrs) [txt text]

  let create_item_icon_meta ?(classes = []) ?(attrs = []) ?(active = false) ?children ()
      =
    let classes = Item_list.CSS.item_meta :: classes in
    let attrs =
      if active then [Svg.Unsafe.string_attrib "data-active" ""] @ attrs else attrs
    in
    let children =
      match children with
      | Some x -> x
      | None ->
          Icon_markup.SVG.
            [ create_path ~d:Svg_icons.check_circle ()
            ; create_path ~d:Svg_icons.close_circle () ]
    in
    Icon_markup.SVG.create ~classes ~attrs ~children ()

  let create_item ?(classes = []) ?attrs ?meta ~primary_text () =
    let classes = CSS.item :: classes in
    let meta =
      match meta with
      | None -> None
      | Some (`Element e) -> Some e
      | Some (`Text s) -> Some (create_item_meta ~text:s ())
    in
    Item_list_markup.create_item ~classes ?attrs ~primary_text ?meta ()

  let create ?(classes = []) ?attrs ?(info : (int * Service.t) option) ?children () =
    let classes = CSS.root :: classes in
    let children =
      match children with
      | Some x -> x
      | None ->
          let meta f =
            match info with
            | None -> `Text not_available
            | Some (_, info) -> f info
          in
          let meta_icon f =
            let active = Option.fold ~none:false ~some:(fun (_, x) -> f x) info in
            `Element (create_item_icon_meta ~active ())
          in
          [ create_item
              ~attrs:[a_user_data "type" "name"]
              ~primary_text:(`Text "Имя")
              ~meta:(meta (fun x -> `Text x.name))
              ()
          ; create_item
              ~attrs:[a_user_data "type" "provider"]
              ~primary_text:(`Text "Провайдер")
              ~meta:(meta (fun x -> `Text x.provider_name))
              ()
          ; create_item
              ~attrs:[a_user_data "type" "type"]
              ~primary_text:(`Text "Тип")
              ~meta:
                (meta (fun x -> `Text (MPEG_TS.service_type_to_string x.service_type)))
              ()
          ; create_item
              ~attrs:[a_user_data "type" "eit-schedule"]
              ~primary_text:(`Text "EIT schedule")
              ~meta:(meta_icon (fun x -> x.eit_schedule))
              ()
          ; create_item
              ~attrs:[a_user_data "type" "scrambling"]
              ~primary_text:(`Text "Скремблирование")
              ~meta:(meta_icon (fun x -> x.free_ca_mode))
              ()
          ; create_item
              ~attrs:[a_user_data "type" "eit-pf"]
              ~primary_text:(`Text "EIT P/F")
              ~meta:(meta_icon (fun x -> x.eit_pf))
              ()
          ; create_item
              ~attrs:[a_user_data "type" "running-status"]
              ~primary_text:(`Text "Running status")
              ~meta:
                (meta (fun x ->
                     `Text (MPEG_TS.running_status_to_string x.running_status)))
              () ]
    in
    Item_list_markup.create
      ~classes
      ?attrs
      ~dense:true
      ~non_interactive:true
      ~children
      ()
end

module Markup = Make (Tyxml.Xml) (Tyxml.Svg) (Tyxml.Html)
