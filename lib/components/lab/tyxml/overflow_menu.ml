open Components_tyxml

module CSS = struct
  let root = "mdc-overflow-menu"

  let actions = BEM.add_element root "actions"

  let overflow = BEM.add_element root "overflow"
end

module Make
    (Xml : Xml_sigs.T)
    (Svg : Svg_sigs.T with module Xml := Xml)
    (Html : Html_sigs.T with module Xml := Xml and module Svg := Svg) =
struct
  open Html
  module Icon = Icon.Make (Xml) (Svg) (Html)
  module Icon_button = Icon_button.Make (Xml) (Svg) (Html)

  let ( ^:: ) x l =
    match x with
    | None -> l
    | Some x -> Xml.W.cons x l

  let overflow ?(classes = []) ?a ?icon () =
    let classes = Top_app_bar.CSS.action_item :: classes in
    let icon =
      match icon with
      | Some x -> x
      | None ->
          Xml.W.return @@ Icon.SVG.(icon ~d:(Xml.W.return Svg_icons.dots_vertical) ())
    in
    Icon_button.icon_button ~classes ?a ~icon ()

  let overflow_menu
      ?(classes = [])
      ?(a = [])
      ?(overflow = Xml.W.return @@ overflow ())
      ?menu
      ~actions
      () : 'a elt =
    let classes = Xml.W.return (CSS.root :: classes) in
    div
      ~a:(a_class classes :: a)
      Xml.W.(
        cons
          (return @@ div ~a:[a_class (return [CSS.actions])] actions)
          (cons
             (return
             @@ div
                  ~a:[a_class (return [CSS.overflow])]
                  (cons overflow (menu ^:: nil ())))
             (nil ())))
end

module Markup = Make (Tyxml.Xml) (Tyxml.Svg) (Tyxml.Html)
