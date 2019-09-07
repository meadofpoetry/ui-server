let string_of_float (f : float) : string = Printf.sprintf "%g" f

let ( @? ) a b =
  match b with
  | None -> a
  | Some b -> a @ [b]

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
    (Html : Html_sigs.NoWrap with module Xml := Xml and module Svg := Svg) =
struct
  open Html

  let create_track_before ?(classes = []) ?(attrs = []) () =
    let classes = CSS.track :: CSS.track_before :: classes in
    div ~a:([a_class classes] @ attrs) []

  let create_track_after ?(classes = []) ?(attrs = []) () =
    let classes = CSS.track :: CSS.track_after :: classes in
    div ~a:([a_class classes] @ attrs) []

  let create_pin_value_marker ?(classes = []) ?(attrs = []) () =
    let classes = CSS.pin_value_marker :: classes in
    span ~a:([a_class classes] @ attrs) []

  let create_pin
      ?(classes = [])
      ?(attrs = [])
      ?(pin_value_marker = create_pin_value_marker ())
      () =
    let classes = CSS.pin :: classes in
    div ~a:([a_class classes] @ attrs) [pin_value_marker]

  let create_thumb ?(classes = []) ?(attrs = []) () =
    let classes = CSS.thumb :: classes in
    svg
      ~a:Svg.([a_class classes; a_width (12., None); a_height (12., None)] @ attrs)
      Svg.[circle ~a:[a_cx (6., None); a_cy (6., None); a_r (6., None)] []]

  let create_focus_ring ?(classes = []) ?(attrs = []) () =
    let classes = CSS.focus :: classes in
    div ~a:([a_class classes] @ attrs) []

  let create_thumb_container
      ?(classes = [])
      ?(attrs = [])
      ?(discrete = false)
      ?pin
      ?(focus_ring = create_focus_ring ())
      ?(thumb = create_thumb ())
      () =
    let classes = CSS.thumb_container :: classes in
    let pin =
      match pin with
      | Some _ as x -> x
      | None -> if discrete then Some (create_pin ()) else None
    in
    div ~a:([a_class classes] @ attrs) (Utils.cons_option pin [thumb; focus_ring])

  let create_track_marker_container ?(classes = []) ?(attrs = []) () =
    let classes = CSS.track_marker_container :: classes in
    div ~a:([a_class classes] @ attrs) []

  let create_container
      ?(classes = [])
      ?(attrs = [])
      ?(discrete = false)
      ?(markers = false)
      ?(track_before = create_track_before ())
      ?(track_after = create_track_after ())
      ?(thumb_container = create_thumb_container ~discrete ())
      ?track_marker_container
      () =
    let classes = CSS.container :: classes in
    let discrete = if markers then true else discrete in
    let track_marker_container =
      match track_marker_container with
      | Some _ as x -> x
      | None ->
          if discrete && markers then Some (create_track_marker_container ()) else None
    in
    div
      ~a:([a_class classes] @ attrs)
      ([track_before; thumb_container; track_after] @? track_marker_container)

  let create
      ?(classes = [])
      ?(attrs = [])
      ?(discrete = false)
      ?(markers = false)
      ?(disabled = false)
      ?label
      ?step
      ?track_before
      ?track_after
      ?thumb_container
      ?track_marker_container
      ?(container =
        create_container
          ~discrete
          ~markers
          ?track_before
          ?track_after
          ?thumb_container
          ?track_marker_container
          ())
      ?(min = 0.)
      ?(max = 100.)
      ?(value = 0.)
      () : 'a elt =
    let classes =
      classes
      |> Utils.cons_if discrete CSS.discrete
      |> Utils.cons_if (discrete && markers) CSS.display_markers
      |> List.cons CSS.root
    in
    div
      ~a:
        ([ a_class classes
         ; a_tabindex 0
         ; a_role ["slider"]
         ; a_aria "valuemin" [string_of_float min]
         ; a_aria "valuemax" [string_of_float max]
         ; a_aria "valuenow" [string_of_float value] ]
         @ attrs
        |> Utils.map_cons_option (fun x -> a_aria "label" [x]) label
        |> Utils.(map_cons_option (a_user_data "step" % string_of_float) step)
        |> Utils.cons_if disabled @@ a_aria "disabled" ["true"])
      [container]
end

module Markup = Make (Tyxml.Xml) (Tyxml.Svg) (Tyxml.Html)
