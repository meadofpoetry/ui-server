let string_of_float (f : float) : string =
  Printf.sprintf "%g" f

module CSS = struct
  let root = "mdc-slider"
  let container = BEM.add_element root "container"
  let track = BEM.add_element root "track"
  let track_marker_container = BEM.add_element root "track-marker-container"
  let track_marker = BEM.add_element root "track-marker"
  let thumb_container = BEM.add_element root "thumb-container"
  let thumb = BEM.add_element root "thumb"
  let focus_ring = BEM.add_element root "focus-ring"
  let pin = BEM.add_element root "pin"
  let pin_value_marker = BEM.add_element root "pin-value-marker"

  let track_before = BEM.add_modifier track "before"
  let track_after = BEM.add_modifier track "after"
  let vertical = BEM.add_modifier root "vertical"
  let active = BEM.add_modifier root "active"
  let disabled = BEM.add_modifier root "disabled"
  let discrete = BEM.add_modifier root "discrete"
  let focus = BEM.add_modifier root "focus"
  let display_markers = BEM.add_modifier root "display-markers"
end

module Make
         (Xml : Xml_sigs.NoWrap)
         (Svg : Svg_sigs.NoWrap with module Xml := Xml)
         (Html : Html_sigs.NoWrap
          with module Xml := Xml
           and module Svg := Svg) = struct
  open Html
  open Utils

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
            |> map_cons_option (a_user_data "step" % string_of_float) step
            |> cons_if disabled @@ a_aria "disabled" ["true"]
            <@> attrs)
      [div ~a:[a_class [CSS.container]]
         ([ div ~a:[a_class [CSS.track; CSS.track_before]] []
          ; div ~a:([a_class [CSS.thumb_container]])
              ([ svg ~a:Svg.[ a_class [CSS.thumb]
                            ; a_width (12., None)
                            ; a_height (12., None)]
                   Svg.[circle ~a:[ a_cx (6., None)
                                  ; a_cy (6., None)
                                  ; a_r (6., None)] []]
               ; div ~a:[a_class [CSS.focus_ring]] []]
               |> cons_if discrete
                    (div ~a:[a_class [CSS.pin]]
                       [span ~a:[a_class [CSS.pin_value_marker]] []]))
          ; div ~a:[a_class [CSS.track; CSS.track_after]] []]
          |> (fun elts ->
            if discrete && markers
            then
              let elt = div ~a:[a_class [CSS.track_marker_container]] [] in
              elts @ [elt]
            else elts))]

end
