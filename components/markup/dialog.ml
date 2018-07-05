open Utils

module Make(Xml : Xml_sigs.NoWrap)
         (Svg : Svg_sigs.NoWrap with module Xml := Xml)
         (Html : Html_sigs.NoWrap
          with module Xml := Xml
           and module Svg := Svg) = struct
  open Html

  let base_class     = "mdc-dialog"
  let surface_class  = CSS.add_element base_class "surface"
  let backdrop_class = CSS.add_element base_class "backdrop"

  module Header = struct

    let _class = CSS.add_element base_class "header"
    let title_class  = CSS.add_element _class "title"

    let create ?(classes=[]) ?attrs ~title () =
      header ~a:([ a_class [_class]])
        [ h2 ~a:([ a_class (title_class :: classes) ] <@> attrs)
            [ pcdata title ]]
  end

  module Body = struct

    let _class           = CSS.add_element base_class "body"
    let scrollable_class = CSS.add_modifier _class "scrollable"

    let create ?(classes=[]) ?attrs ?(scrollable=false) ~content () =
      section ~a:([ a_class (classes
                             |> cons_if scrollable scrollable_class
                             |> List.cons _class) ] <@> attrs)
        content

  end

  module Footer = struct

    let _class = CSS.add_element base_class "footer"
    let button_class = CSS.add_element _class "button"
    let accept_button_class = CSS.add_modifier button_class "accept"
    let cancel_button_class = CSS.add_modifier button_class "cancel"

    let create ?(classes=[]) ?attrs ~children () =
      footer ~a:([ a_class (_class :: classes)] <@> attrs) children

  end

  let create ?(classes=[]) ?attrs ?label_id ?description_id ~content () =
    aside ~a:([ a_class (base_class :: classes)
              ; a_role ["alertdialog"]]
              |> map_cons_option (fun x -> a_aria "labelledby" [x]) label_id
              |> map_cons_option (fun x -> a_aria "describedby" [x]) description_id
              <@> attrs)
      [ div ~a:([a_class [surface_class]]) content
      ; div ~a:([a_class [backdrop_class]]) [] ]

end
