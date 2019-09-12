open Js_of_ocaml
open Js_of_ocaml_tyxml
open Components
open Application_types
open Board_niitv_tsan_types
include Board_niitv_tsan_widgets_tyxml.Service_sdt_info
module Markup_js = Make (Tyxml_js.Xml) (Tyxml_js.Svg) (Tyxml_js.Html)

type event = [`Service of (int * Service.t) option]

module Attr = struct
  type typ =
    [ `Name
    | `Provider
    | `Service_type
    | `Scrambling
    | `Eit_schedule
    | `Eit_pf
    | `Running_status ]

  let data_active = "data-active"

  let data_type = "data-type"

  let get_type (elt : #Dom_html.element Js.t) : typ option =
    Js.Opt.case
      (elt##getAttribute (Js.string data_type))
      (fun () -> None)
      (fun s ->
        match Js.to_string s with
        | "name" -> Some `Name
        | "provider" -> Some `Provider
        | "type" -> Some `Service_type
        | "eit-schedule" -> Some `Eit_schedule
        | "eit-pf" -> Some `Eit_pf
        | "scrambling" -> Some `Scrambling
        | "running-status" -> Some `Running_status
        | _ -> None)
end

module Selector = struct
  let item = Printf.sprintf ".%s[%s]" CSS.item Attr.data_type

  let meta = "." ^ Item_list.CSS.item_meta
end

let set_meta_text meta text = meta##.textContent := Js.some @@ Js.string text

let set_meta_active meta active =
  if active
  then Element.set_attribute meta Attr.data_active ""
  else Element.remove_attribute meta Attr.data_active

let set_item_not_available item =
  Js.Opt.iter
    (item##querySelector (Js.string Selector.meta))
    (fun meta ->
      if Element.has_class meta Icon.CSS.root
      then set_meta_active meta false
      else set_meta_text meta not_available)

class t (elt : Dom_html.element Js.t) () =
  object (self)
    inherit Widget.t elt () as super

    method notify : event -> unit =
      function
      | `Service info -> self#set_service_info info

    method set_service_info info =
      let items = Element.query_selector_all super#root Selector.item in
      match info with
      | None -> List.iter set_item_not_available items
      | Some (_, info) ->
          List.iter
            (fun item ->
              match Attr.get_type item with
              | None -> ()
              | Some typ -> self#set_item_value item typ info)
            items

    method private set_meta_value meta (info : Service.t) =
      function
      | `Name -> set_meta_text meta info.name
      | `Provider -> set_meta_text meta info.provider_name
      | `Service_type ->
          let text = MPEG_TS.service_type_to_string info.service_type in
          set_meta_text meta text
      | `Eit_schedule -> set_meta_active meta info.eit_schedule
      | `Scrambling -> set_meta_active meta info.free_ca_mode
      | `Eit_pf -> set_meta_active meta info.eit_pf
      | `Running_status ->
          let text = MPEG_TS.running_status_to_string info.running_status in
          set_meta_text meta text

    method private set_item_value item typ info =
      let meta =
        match Element.query_selector item Selector.meta with
        | Some x -> x
        | None ->
            let meta =
              Tyxml_js.To_dom.of_element
                (match typ with
                | `Eit_schedule | `Eit_pf | `Scrambling ->
                    Markup_js.create_item_icon_meta ()
                | `Name | `Provider | `Service_type | `Running_status ->
                    Markup_js.create_item_meta ~text:"" ())
            in
            Dom.appendChild item meta;
            meta
      in
      self#set_meta_value meta info typ
  end

let attach elt = new t (elt :> Dom_html.element Js.t) ()

let make ?classes ?attrs ?info ?children () =
  Markup_js.create ?classes ?attrs ?info ?children () |> Tyxml_js.To_dom.of_ul |> attach
