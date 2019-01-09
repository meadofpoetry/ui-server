open Utils

module Make(Xml : Xml_sigs.NoWrap)
         (Svg : Svg_sigs.NoWrap with module Xml := Xml)
         (Html : Html_sigs.NoWrap
          with module Xml := Xml
           and module Svg := Svg) = struct
  open Html

  module Divider = Divider.Make(Xml)(Svg)(Html)
  module Icon = Icon.Make(Xml)(Svg)(Html)

  let base_class = "mdc-expansion-panel"
  let expanded_class = CSS.add_modifier base_class "expanded"
  let panel_wrapper_class = CSS.add_element base_class "panel-wrapper"

  module Primary = struct

    let _class = CSS.add_element base_class "primary"
    let summary_class = CSS.add_element base_class "summary"
    let details_class = CSS.add_element base_class "details"
    let heading_class = CSS.add_element base_class "heading"
    let icon_class = CSS.add_element base_class "icon"

    let create ?(classes = []) ?attrs
          ?(heading_details = []) ?(details = []) ~title () : 'a elt =
      div ~a:([ a_class (_class :: classes)
              ; a_tabindex 0 ] <@> attrs)
        [ div ~a:[a_class [summary_class]]
            [ div ~a:[a_class [heading_class]]
                ((match heading_details with
                  | [] -> None
                  | l -> Some (div ~a:[a_class [details_class]] l))
                 |> (fun x -> cons_option x [])
                 |> List.cons (txt title))
            ; div ~a:[a_class [details_class]] details
            ]
        ; div ~a:[a_class [icon_class]; a_tabindex (-1)]
            [Icon.Font.create ~icon:"expand_more" ()]
        ]

  end

  module Actions = struct
    let _class = CSS.add_element base_class "actions"
    let action_class = CSS.add_element base_class "action"

    let create ?(classes = []) ?attrs ~actions () : 'a elt =
      div ~a:([a_class (_class :: classes)] <@> attrs) actions
  end

  module Panel = struct
    let _class = CSS.add_element base_class "panel"

    let create ?(classes = []) ?attrs ~content () : 'a elt =
      div ~a:([a_class (_class :: classes)] <@> attrs) content
  end

  let create ?(classes=[]) ?attrs ?actions ~primary ~panel () : 'a elt =
    div ~a:([a_class (base_class :: classes)] <@> attrs)
      [ primary
      ; div ~a:[a_class [panel_wrapper_class]]
          (match actions with
           | None -> [panel]
           | Some x -> panel :: Divider.create () :: x :: [])]
end
