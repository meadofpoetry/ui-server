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
    (Xml : Xml_sigs.T with type ('a, 'b) W.ft = 'a -> 'b)
    (Svg : Svg_sigs.T with module Xml := Xml)
    (Html : Html_sigs.T with module Xml := Xml and module Svg := Svg) =
struct
  open Xml.W
  open Html

  let ( % ) f g x = f (g x)

  let ( @:: ) = cons

  let ( ^:: ) x l = Option.fold ~none:l ~some:(fun x -> cons x l) x

  let slider_track_before ?(classes = return []) ?(a = []) ?(children = nil ()) () =
    let classes = fmap (fun x -> CSS.track :: CSS.track_before :: x) classes in
    div ~a:(a_class classes :: a) children

  let slider_track_after ?(classes = return []) ?(a = []) ?(children = nil ()) () =
    let classes = fmap (fun x -> CSS.track :: CSS.track_after :: x) classes in
    div ~a:(a_class classes :: a) children

  let slider_pin_value_marker ?(classes = return []) ?(a = []) ?(children = nil ()) () =
    let classes = fmap (List.cons CSS.pin_value_marker) classes in
    span ~a:(a_class classes :: a) children

  let slider_pin
      ?(classes = return [])
      ?(a = [])
      ?(pin_value_marker = slider_pin_value_marker ())
      () =
    let classes = fmap (fun x -> CSS.pin :: x) classes in
    div ~a:(a_class classes :: a) (singleton (return pin_value_marker))

  let slider_thumb ?(classes = return []) ?(a = []) () =
    let classes = fmap (fun x -> CSS.thumb :: x) classes in
    svg
      ~a:
        (Svg.
           [a_class classes; a_width (return (12., None)); a_height (return (12., None))]
        @ a)
      Svg.(
        singleton
          (return
             (circle
                ~a:
                  [ a_cx (return (6., None))
                  ; a_cy (return (6., None))
                  ; a_r (return (6., None)) ]
                (nil ()))))

  let slider_focus_ring ?(classes = return []) ?(a = []) ?(children = nil ()) () =
    let classes = fmap (fun x -> CSS.focus :: x) classes in
    div ~a:(a_class classes :: a) children

  let slider_thumb_container
      ?(classes = return [])
      ?(a = [])
      ?(discrete = false)
      ?pin
      ?(focus_ring = slider_focus_ring ())
      ?(thumb = slider_thumb ())
      () =
    let classes = fmap (fun x -> CSS.thumb_container :: x) classes in
    let pin =
      match pin with
      | Some x -> Some (return x)
      | None -> if discrete then Some (return (slider_pin ())) else None
    in
    div ~a:(a_class classes :: a) (pin ^:: return thumb @:: return focus_ring @:: nil ())

  let slider_track_marker_container
      ?(classes = return [])
      ?(a = [])
      ?(children = nil ())
      () =
    let classes = fmap (fun x -> CSS.track_marker_container :: x) classes in
    div ~a:(a_class classes :: a) children

  let slider_container
      ?(classes = return [])
      ?(a = [])
      ?(discrete = false)
      ?(markers = false)
      ?(track_before = slider_track_before ())
      ?(track_after = slider_track_after ())
      ?(thumb_container = slider_thumb_container ~discrete ())
      ?track_marker_container
      () =
    let classes = fmap (fun x -> CSS.container :: x) classes in
    let discrete = if markers then true else discrete in
    let track_marker_container =
      match track_marker_container with
      | Some x -> Some (return x)
      | None ->
          if discrete && markers
          then Some (return @@ slider_track_marker_container ())
          else None
    in
    div
      ~a:(a_class classes :: a)
      (return track_before
      @:: return thumb_container
      @:: return track_after
      @:: track_marker_container
      ^:: nil ())

  let slider
      ?(classes = return [])
      ?(a = [])
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
        slider_container
          ~discrete
          ~markers
          ?track_before
          ?track_after
          ?thumb_container
          ?track_marker_container
          ())
      ?(min = return 0.)
      ?(max = return 100.)
      ?(value = return 0.)
      () : 'a elt =
    let classes =
      fmap
        (Utils.cons_if discrete CSS.discrete
        % Utils.cons_if (discrete && markers) CSS.display_markers
        % List.cons CSS.root)
        classes
    in
    div
      ~a:
        (a_class classes
         :: a_tabindex (return 0)
         :: a_role (return ["slider"])
         :: a_aria "valuemin" (fmap (fun x -> [string_of_float x]) min)
         :: a_aria "valuemax" (fmap (fun x -> [string_of_float x]) max)
         :: a_aria "valuenow" (fmap (fun x -> [string_of_float x]) value)
         :: a
        |> Utils.map_cons_option (fun x -> a_aria "label" (fmap (fun x -> [x]) x)) label
        |> Utils.(map_cons_option (a_user_data "step" % fmap string_of_float) step)
        |> Utils.cons_if disabled @@ a_aria "disabled" (return ["true"]))
      (singleton (return container))
end

module F = Make (Tyxml.Xml) (Tyxml.Svg) (Tyxml.Html)
