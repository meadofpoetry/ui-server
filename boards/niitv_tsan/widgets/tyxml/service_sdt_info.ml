open Components_tyxml
open Application_types
open Board_niitv_tsan_types

module CSS = struct
  let root = Util.CSS.root ^ "-service-sdt-info"

  let item = BEM.add_element root "item"

  let meta_active = BEM.add_element Item_list.CSS.item_meta "active"
end

let not_available = "n/a"

module Make
    (Xml : Xml_sigs.T with type ('a, 'b) W.ft = 'a -> 'b)
    (Svg : Svg_sigs.T with module Xml := Xml)
    (Html : Html_sigs.T with module Xml := Xml and module Svg := Svg) =
struct
  open Xml.W
  open Html

  open Item_list.Make (Xml) (Svg) (Html)

  open Icon.Make (Xml) (Svg) (Html)

  let ( @:: ) x l = cons (return x) l

  let create_item_meta ?(a = []) ~text () =
    let text = return (txt text) in
    span ~a:(a_class (return [Item_list.CSS.item_meta]) :: a) (singleton text)

  let create_item_icon_meta ?a ?(active = return false) () =
    let classes =
      fmap (fun x -> Utils.cons_if x CSS.meta_active [Item_list.CSS.item_meta]) active
    in
    let children =
      SVG.icon_path
        ~d:
          (fmap
             (fun x -> if x then Svg_icons.check_circle else Svg_icons.close_circle)
             active)
        ()
      @:: nil ()
    in
    SVG.icon ~classes ?a ~children ()

  let create_item ?a ?meta ~primary_text () =
    let classes = return [CSS.item] in
    let meta =
      match meta with
      | None -> None
      | Some (`Element e) -> Some (return e)
      | Some (`Text s) -> Some (return (create_item_meta ~text:s ()))
    in
    list_item ~classes ?a ~primary_text ?meta ()

  let create ?a ?(info : (int * Service.t) option wrap = return None) ?children () =
    let classes = return [CSS.root] in
    let children =
      match children with
      | Some x -> x
      | None ->
          let meta f =
            `Text
              (fmap
                 (function
                   | None -> not_available
                   | Some (_, (info : Service.t)) -> f info)
                 info)
          in
          let meta_icon f =
            let active =
              fmap
                (function
                  | None -> false
                  | Some (_, x) -> f x)
                info
            in
            `Element (create_item_icon_meta ~active ())
          in
          create_item
            ~a:[a_user_data "type" (return "name")]
            ~primary_text:(`Text (return "Имя"))
            ~meta:(meta (fun x -> x.name))
            ()
          @:: create_item
                ~a:[a_user_data "type" (return "provider")]
                ~primary_text:(`Text (return "Провайдер"))
                ~meta:(meta (fun x -> x.provider_name))
                ()
          @:: create_item
                ~a:[a_user_data "type" (return "type")]
                ~primary_text:(`Text (return "Тип"))
                ~meta:(meta (fun x -> MPEG_TS.service_type_to_string x.service_type))
                ()
          @:: create_item
                ~a:[a_user_data "type" (return "eit-schedule")]
                ~primary_text:(`Text (return "EIT schedule"))
                ~meta:(meta_icon (fun x -> x.eit_schedule))
                ()
          @:: create_item
                ~a:[a_user_data "type" (return "scrambling")]
                ~primary_text:(`Text (return "Скремблирование"))
                ~meta:(meta_icon (fun x -> x.free_ca_mode))
                ()
          @:: create_item
                ~a:[a_user_data "type" (return "eit-pf")]
                ~primary_text:(`Text (return "EIT P/F"))
                ~meta:(meta_icon (fun x -> x.eit_pf))
                ()
          @:: create_item
                ~a:[a_user_data "type" (return "running-status")]
                ~primary_text:(`Text (return "Running status"))
                ~meta:(meta (fun x -> MPEG_TS.running_status_to_string x.running_status))
                ()
          @:: nil ()
    in
    list ~classes ?a ~dense:true ~non_interactive:true ~children ()
end

module F = Make (Tyxml.Xml) (Tyxml.Svg) (Tyxml.Html)
