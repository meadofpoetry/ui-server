open Utils

module Make
         (Xml : Xml_sigs.NoWrap)
         (Svg : Svg_sigs.NoWrap with module Xml := Xml)
         (Html : Html_sigs.NoWrap
          with module Xml := Xml
           and module Svg := Svg) = struct
  open Html

  module CSS = struct
    include CSS

    let root = "mdc-slider"
    let track_container = add_element root "track-container"
    let track = add_element root "track"
    let track_marker_container = add_element root "track-marker-container"
    let track_marker = add_element root "track-marker"
    let thumb_container = add_element root "thumb-container"
    let thumb = add_element root "thumb"
    let focus_ring = add_element root "focus-ring"
    let pin = add_element root "pin"
    let pin_value_marker = add_element root "pin-value-marker"

    let active = add_modifier root "active"
    let disabled = add_modifier root "disabled"
    let discrete = add_modifier root "discrete"
    let focus = add_modifier root "focus"
    let in_transit = add_modifier root "in-transit"
    let display_markers = add_modifier root "display-markers"
  end

  let string_of_float (f : float) : string =
    Printf.sprintf "%g" f

  let create ?(classes = []) ?attrs ?(discrete = false)
        ?(markers = false) ?(disabled = false)
        ?label ?step ?(min = 0.) ?(max = 100.) ?(value = 0.) () : 'a elt =
    let discrete = if markers then true else discrete in
    let classes =
      classes
      |> cons_if discrete CSS.discrete
      |> cons_if (discrete && markers) CSS.display_markers
      |> List.cons CSS.root in
    div ~a:([ a_class classes
            ; a_tabindex 0
            ; a_role ["slider"]
            ; a_aria "valuemin" [string_of_float min]
            ; a_aria "valuemax" [string_of_float max]
            ; a_aria "valuenow" [string_of_float value]]
            |> map_cons_option (fun x -> a_aria "label" [x]) label
            |> map_cons_option CCFun.(a_user_data "step" % string_of_float) step
            |> cons_if disabled @@ a_aria "disabled" ["true"]
            <@> attrs)
      [div ~a:[a_class [CSS.track_container]]
         (cons_if (discrete && markers)
            (div ~a:[a_class [CSS.track_marker_container]] []) []
          |> List.cons @@ div ~a:[a_class [CSS.track]] [])
      ; div ~a:([a_class [CSS.thumb_container]])
          ([ svg ~a:Svg.[ a_class [CSS.thumb]
                        ; a_width (21., None)
                        ; a_height (21., None)]
               Svg.[circle ~a:[ a_cx (10.5, None)
                              ; a_cy (10.5, None)
                              ; a_r (7.875, None)] []]
           ; div ~a:[a_class [CSS.focus_ring]] []]
           |> cons_if discrete
                (div ~a:[a_class [CSS.pin]]
                   [span ~a:[a_class [CSS.pin_value_marker]] []]))
      ]

end
