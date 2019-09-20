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
    (Xml : Xml_sigs.T with type ('a, 'b) W.ft = 'a -> 'b)
    (Svg : Svg_sigs.T with module Xml := Xml)
    (Html : Html_sigs.T with module Xml := Xml and module Svg := Svg) =
struct
  open Xml.W
  open Html

  let ( % ) f g x = f (g x)

  let ( @:: ) = cons

  let linear_progress_buffering_dots
      ?(classes = return [])
      ?(a = [])
      ?(children = nil ())
      () =
    let classes = fmap (List.cons CSS.buffering_dots) classes in
    div ~a:(a_class classes :: a) children

  let linear_progress_buffer ?(classes = return []) ?(a = []) ?(children = nil ()) () =
    let classes = fmap (List.cons CSS.buffer) classes in
    div ~a:(a_class classes :: a) children

  let linear_progress_bar_inner ?(classes = return []) ?(a = []) ?(children = nil ()) ()
      =
    let classes = fmap (List.cons CSS.bar_inner) classes in
    span ~a:(a_class classes :: a) children

  let linear_progress_primary_bar
      ?(classes = return [])
      ?(a = [])
      ?(children = singleton (return (linear_progress_bar_inner ())))
      () =
    let classes = fmap (fun x -> CSS.bar :: CSS.primary_bar :: x) classes in
    div ~a:(a_class classes :: a) children

  let linear_progress_secondary_bar
      ?(classes = return [])
      ?(a = [])
      ?(children = singleton (return (linear_progress_bar_inner ())))
      () =
    let classes = fmap (fun x -> CSS.bar :: CSS.secondary_bar :: x) classes in
    div ~a:(a_class classes :: a) children

  let linear_progress
      ?(classes = return [])
      ?(a = [])
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
      fmap
        (Utils.cons_if closed CSS.closed
        % Utils.cons_if indeterminate CSS.indeterminate
        % Utils.cons_if reversed CSS.reversed
        % List.cons CSS.root)
        classes
    in
    let children =
      match children with
      | Some x -> x
      | None ->
          let opt_get_lazy ~default o =
            match o with
            | None -> return (default ())
            | Some x -> return x
          in
          opt_get_lazy ~default:linear_progress_buffering_dots buffering_dots
          @:: opt_get_lazy ~default:linear_progress_buffer buffer
          @:: opt_get_lazy ~default:linear_progress_primary_bar primary_bar
          @:: opt_get_lazy ~default:linear_progress_secondary_bar secondary_bar
          @:: nil ()
    in
    div ~a:(a_role (return ["progressbar"]) :: a_class classes :: a) children
end
