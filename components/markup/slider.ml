open Utils

module Make
         (Xml : Xml_sigs.NoWrap)
         (Svg : Svg_sigs.NoWrap with module Xml := Xml)
         (Html : Html_sigs.NoWrap
          with module Xml := Xml
           and module Svg := Svg) = struct
  open Html

  let base_class                   = "mdc-slider"
  let track_container_class        = CSS.add_element base_class "track-container"
  let track_class                  = CSS.add_element base_class "track"
  let track_marker_container_class = CSS.add_element base_class "track-marker-container"
  let thumb_container_class        = CSS.add_element base_class "thumb-container"
  let thumb_class                  = CSS.add_element base_class "thumb"
  let focus_ring_class             = CSS.add_element base_class "focus-ring"
  let pin_class                    = CSS.add_element base_class "pin"
  let pin_value_marker_class       = CSS.add_element base_class "pin-value-marker"

  let create ?(classes=[]) ?attrs ?(discrete=false) ?(markers=false) ?(disabled=false)
        ?label ?step ?(min=0.) ?(max=100.) ?(value=0.) () =
    let discrete = if markers then true else discrete in
    div ~a:([ a_class (classes
                       |> cons_if discrete @@ CSS.add_modifier base_class "discrete"
                       |> cons_if (discrete && markers) @@ CSS.add_modifier base_class "display-markers"
                       |> List.cons base_class)
            ; a_tabindex 0
            ; a_role ["slider"]
            ; a_aria "valuemin" [ string_of_float min ]
            ; a_aria "valuemax" [ string_of_float max ]
            ; a_aria "valuenow" [ string_of_float value ] ]
            |> map_cons_option (fun x -> a_aria "label" [x]) label
            |> map_cons_option (fun x -> a_user_data "step" (string_of_float x)) step
            |> cons_if disabled @@ a_aria "disabled" ["true"]
            <@> attrs)
      [ div ~a:([ a_class [track_container_class]])
          (cons_if (discrete && markers) (div ~a:[ a_class [track_marker_container_class]] []) []
           |> List.cons @@ div ~a:([ a_class [track_class]]) [])
      ; div ~a:([ a_class [thumb_container_class]])
          ([ svg ~a:([ Svg.a_class [thumb_class]
                     ; Svg.a_width (21., None)
                     ; Svg.a_height (21., None)])
               [ Svg.circle ~a:[ Svg.a_cx (10.5, None)
                               ; Svg.a_cy (10.5, None)
                               ; Svg.a_r (7.875, None)] []]
           ; div ~a:([ a_class [focus_ring_class]]) []]
           |> cons_if discrete (div ~a:[a_class [pin_class]]
                                  [span ~a:[a_class [pin_value_marker_class]] []]))
      ]

end
