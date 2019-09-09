module CSS = struct
  let root = "mdc-linear-progress"

  let buffering_dots = BEM.add_element root "buffering-dots"

  let buffer = BEM.add_element root "buffer"

  let bar = BEM.add_element root "bar"

  let primary_bar = BEM.add_element root "primary-bar"

  let secondary_bar = BEM.add_element root "secondary-bar"

  let bar_inner = BEM.add_element root "bar-inner"

  (** Puts the linear progress indicator in an indeterminate state. *)
  let indeterminate = BEM.add_modifier root "indeterminate"

  (** Reverses the direction of the linear progress indicator. *)
  let reversed = BEM.add_modifier root "reversed"

  (** Hides the linear progress indicator. *)
  let closed = BEM.add_modifier root "closed"
end

module Make
    (Xml : Xml_sigs.NoWrap)
    (Svg : Svg_sigs.NoWrap with module Xml := Xml)
    (Html : Html_sigs.NoWrap with module Xml := Xml and module Svg := Svg) =
struct
  open Html

  let create_buffering_dots ?(classes = []) ?(attrs = []) ?(children = []) () =
    let classes = CSS.buffering_dots :: classes in
    div ~a:([a_class classes] @ attrs) children

  let create_buffer ?(classes = []) ?(attrs = []) ?(children = []) () =
    let classes = CSS.buffer :: classes in
    div ~a:([a_class classes] @ attrs) children

  let create_bar_inner ?(classes = []) ?(attrs = []) ?(children = []) () =
    let classes = CSS.bar_inner :: classes in
    span ~a:([a_class classes] @ attrs) children

  let create_primary_bar
      ?(classes = [])
      ?(attrs = [])
      ?(children = [create_bar_inner ()])
      () =
    let classes = CSS.bar :: CSS.primary_bar :: classes in
    div ~a:([a_class classes] @ attrs) children

  let create_secondary_bar
      ?(classes = [])
      ?(attrs = [])
      ?(children = [create_bar_inner ()])
      () =
    let classes = CSS.bar :: CSS.secondary_bar :: classes in
    div ~a:([a_class classes] @ attrs) children

  let create
      ?(classes = [])
      ?(attrs = [])
      ?(indeterminate = false)
      ?(reversed = false)
      ?(closed = false)
      ?buffering_dots
      ?buffer
      ?primary_bar
      ?secondary_bar
      ?children
      () : 'a elt =
    let classes =
      classes
      |> Utils.cons_if closed CSS.closed
      |> Utils.cons_if indeterminate CSS.indeterminate
      |> Utils.cons_if reversed CSS.reversed
      |> List.cons CSS.root
    in
    let children =
      match children with
      | Some x -> x
      | None ->
          let opt_get_lazy ~default o =
            match o with
            | None -> default ()
            | Some x -> x
          in
          [ opt_get_lazy ~default:create_buffering_dots buffering_dots
          ; opt_get_lazy ~default:create_buffer buffer
          ; opt_get_lazy ~default:create_primary_bar primary_bar
          ; opt_get_lazy ~default:create_secondary_bar secondary_bar ]
    in
    div ~a:([a_role ["progressbar"]; a_class classes] @ attrs) children
end
