open Components_tyxml

module CSS = struct
  let root = Util.CSS.root ^ "-stream-select"

  let label = BEM.add_element root "label"

  let select = BEM.add_element root "select"
end

module Make
    (Xml : Intf.Xml)
    (Svg : Svg_sigs.T with module Xml := Xml)
    (Html : Html_sigs.T with module Xml := Xml and module Svg := Svg) =
struct
  open Xml.W
  open Xml.Wutils
  open Html
  module Select_markup = Select.Make (Xml) (Svg) (Html)

  let ( @:: ) = cons

  let create_label ?(classes = return []) ?(a = []) ?label () =
    let classes = fmap (fun x -> CSS.label :: x) classes in
    let default_text =
      "Выберите поток для отображения данных"
    in
    let text =
      match label with
      | None -> return default_text
      | Some x ->
          fmap
            (function
              | None -> default_text
              | Some x -> x)
            x
    in
    let label = return (txt text) in
    span ~a:(a_class classes :: a) (label @:: nil ())

  let create_select ?(classes = return []) ?(a = []) ?streams () =
    let classes = fmap (fun x -> CSS.select :: x) classes in
    Select_markup.Native.select
      ~a
      ~classes
      ~outlined:true
      ~label:(return "Потоки")
      ()

  let create ?(classes = return []) ?(a = []) ?streams () =
    let classes = fmap (fun x -> CSS.root :: x) classes in
    let label = return (create_label ()) in
    let select = return (create_select ?streams ()) in
    div ~a:(a_class classes :: a) (label @:: select @:: nil ())
end
