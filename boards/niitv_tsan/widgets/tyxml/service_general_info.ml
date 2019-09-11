open Components_tyxml
open Board_niitv_tsan_types

module CSS = struct
  let root = Util.CSS.root ^ "-service-general-info"

  let item = BEM.add_element root "item"
end

module Make
    (Xml : Xml_sigs.NoWrap)
    (Svg : Svg_sigs.NoWrap with module Xml := Xml)
    (Html : Html_sigs.NoWrap with module Xml := Xml and module Svg := Svg) =
struct
  open Html
  module Item_list_markup = Item_list.Make (Xml) (Svg) (Html)

  let create_item_meta ?(classes = []) ?(attrs = []) ~text () =
    span ~a:([a_class classes] @ attrs) [txt text]

  let create_item ?(classes = []) ?attrs ?meta ~primary_text () =
    let classes = CSS.item :: classes in
    let meta =
      match meta with
      | None -> None
      | Some (`Element e) -> Some e
      | Some (`Text s) -> Some (create_item_meta ~text:s ())
    in
    Item_list_markup.create_item ~classes ?attrs ~primary_text ?meta ()

  let create ?(classes = []) ?attrs ?children ?info ?bitrate ?max_bitrate ?min_bitrate ()
      =
    let classes = CSS.root :: classes in
    let children =
      match children with
      | Some x -> x
      | None ->
          let meta ?default f = function
            | None -> (
              match default with
              | None -> `Text ""
              | Some f -> f ())
            | Some x -> f x
          in
          [ create_item
              ~attrs:[a_user_data "type" "service-id"]
              ~primary_text:(`Text "Service ID")
              ~meta:(meta (fun (x, _) -> `Text (string_of_int x)) info)
              ()
          ; create_item
              ~attrs:[a_user_data "type" "pmt-pid"]
              ~primary_text:(`Text "PMT PID")
              ~meta:
                (meta
                   (fun (_, (x : Service_info.t)) -> `Text (string_of_int x.pmt_pid))
                   info)
              ()
          ; create_item
              ~attrs:[a_user_data "type" "pcr-pid"]
              ~primary_text:(`Text "PCR PID")
              ~meta:
                (meta
                   (fun (_, (x : Service_info.t)) -> `Text (string_of_int x.pcr_pid))
                   info)
              ()
          ; create_item
              ~attrs:[a_user_data "type" "bitratenow"]
              ~primary_text:(`Text "Битрейт")
              ~meta:(meta (fun x -> `Text (string_of_int x)) bitrate)
              ()
          ; create_item
              ~attrs:[a_user_data "type" "bitratemin"]
              ~primary_text:(`Text "Min")
              ~meta:(meta (fun x -> `Text (string_of_int x)) min_bitrate)
              ()
          ; create_item
              ~attrs:[a_user_data "type" "bitratemax"]
              ~primary_text:(`Text "Max")
              ~meta:(meta (fun x -> `Text (string_of_int x)) max_bitrate)
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
