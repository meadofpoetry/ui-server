open Components_tyxml

module CSS = struct
  let root = Util.CSS.root ^ "-bitrate-summary"

  let value = BEM.add_element root "value"

  let value_total = BEM.add_modifier value "total"

  let value_effective = BEM.add_modifier value "effective"
end

let na = "n/a"

let bitrate_to_string = function
  | None -> na
  | Some x -> Printf.sprintf "%f Мбит/с" (float_of_int x /. 1_000_000.)

module Make
    (Xml : Intf.Xml)
    (Svg : Svg_sigs.T with module Xml := Xml)
    (Html : Html_sigs.T with module Xml := Xml and module Svg := Svg) =
struct
  open Xml.W
  open Xml.Wutils
  open Html
  module Item_list_markup = Item_list.Make (Xml) (Svg) (Html)

  let create_value ?(classes = return []) ?a ?(value = return None) text =
    let classes = fmap (fun x -> CSS.value :: x) classes in
    let meta =
      return
        (span
           ~a:[ a_class (return [ Item_list.CSS.item_meta ]) ]
           (const [ txt (fmap bitrate_to_string value) ]))
    in
    let primary_text = `Text text in
    Item_list_markup.list_item ~classes ?a ~meta ~primary_text ()

  let create ?(classes = return []) ?a ?(bitrate = return None) () =
    let open Board_niitv_tsan_types.Bitrate in
    let classes = fmap (fun x -> CSS.root :: x) classes in
    let children =
      let total =
        create_value
          ~classes:(return [ CSS.value_total ])
          ~value:
            (fmap
               (function None -> None | Some { total; _ } -> Some total.cur)
               bitrate)
          (return "Общий битрейт:")
      in
      let effective =
        create_value
          ~value:
            (fmap
               (function
                 | None -> None | Some { effective; _ } -> Some effective.cur)
               bitrate)
          ~classes:(return [ CSS.value_effective ])
          (return "Полезный битрейт:")
      in
      const [ total; effective ]
    in
    Item_list_markup.list ~classes ?a ~dense:true ~non_interactive:true
      ~children ()
end

module F = Make (Impl.Xml) (Impl.Svg) (Impl.Html)
