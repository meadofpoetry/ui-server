open Components_tyxml
open Board_niitv_tsan_types

module CSS = struct
  let root = Util.CSS.root ^ "-service-general-info"

  let item = BEM.add_element root "item"
end

let not_available = "n/a"

module Make
    (Xml : Xml_sigs.NoWrap)
    (Svg : Svg_sigs.NoWrap with module Xml := Xml)
    (Html : Html_sigs.NoWrap with module Xml := Xml and module Svg := Svg) =
struct
  open Html

  open Item_list.Make (Xml) (Svg) (Html)

  let create_item_meta ?(classes = []) ?(a = []) ~text () =
    let classes = Item_list.CSS.item_meta :: classes in
    span ~a:(a_class classes :: a) [txt text]

  let create_item ?(classes = []) ?a ?meta ~primary_text () =
    let classes = CSS.item :: classes in
    let meta =
      match meta with
      | None -> None
      | Some (`Element e) -> Some e
      | Some (`Text s) -> Some (create_item_meta ~text:s ())
    in
    list_item ~classes ?a ~primary_text ?meta ()

  let create
      ?(classes = [])
      ?(a = [])
      ?children
      ?(info : (int * Service.t) option)
      ?bitrate
      ?max_bitrate
      ?min_bitrate
      () =
    let classes = CSS.root :: classes in
    let a =
      match info with
      | None | Some (_, {elements = []; _}) -> a
      | Some (_, info) ->
          let elements = Util.service_pids info in
          a_user_data "elements" (String.concat "," @@ List.map string_of_int elements)
          :: a
    in
    let children =
      match children with
      | Some x -> x
      | None ->
          let meta ?default f = function
            | None -> (
              match default with
              | None -> `Text not_available
              | Some f -> f ())
            | Some x -> f x
          in
          [ create_item
              ~a:[a_user_data "type" "service-id"]
              ~primary_text:(`Text "Service ID")
              ~meta:(meta (fun (x, _) -> `Text (string_of_int x)) info)
              ()
          ; create_item
              ~a:[a_user_data "type" "pmt-pid"]
              ~primary_text:(`Text "PMT PID")
              ~meta:
                (meta (fun (_, (x : Service.t)) -> `Text (string_of_int x.pmt_pid)) info)
              ()
          ; create_item
              ~a:[a_user_data "type" "pcr-pid"]
              ~primary_text:(`Text "PCR PID")
              ~meta:
                (meta (fun (_, (x : Service.t)) -> `Text (string_of_int x.pcr_pid)) info)
              ()
          ; create_item
              ~a:[a_user_data "type" "bitratenow"]
              ~primary_text:(`Text "Битрейт")
              ~meta:(meta (fun x -> `Text (string_of_int x)) bitrate)
              ()
          ; create_item
              ~a:[a_user_data "type" "bitratemin"]
              ~primary_text:(`Text "Min")
              ~meta:(meta (fun x -> `Text (string_of_int x)) min_bitrate)
              ()
          ; create_item
              ~a:[a_user_data "type" "bitratemax"]
              ~primary_text:(`Text "Max")
              ~meta:(meta (fun x -> `Text (string_of_int x)) max_bitrate)
              () ]
    in
    list ~classes ~a ~dense:true ~non_interactive:true ~children ()
end

module F = Make (Tyxml.Xml) (Tyxml.Svg) (Tyxml.Html)