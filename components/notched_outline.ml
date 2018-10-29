open Containers
open Tyxml_js

module Markup = Components_markup.Notched_outline.Make(Xml)(Svg)(Html)

class t () =
  let path = Markup.create_path () in
  let svg = Markup.create_svg path () in
  let elt = Markup.create svg () |> To_dom.of_element in
  object(self)

    inherit Widget.t elt ()

    (** Adds the outline notched selector and updates the notch width
     * calculated based off of notch_width
     *)
    method notch (notch_width : float) : unit =
      self#add_class Markup.notched_class;
      self#update_svg_path notch_width

    method close_notch () : unit =
      self#remove_class Markup.notched_class

    (* Private methods *)

    (** Updates the SVG path of the focus outline element based on the notchWidth
     * and the RTL context.
     *)
    method private update_svg_path (notch_width : float) : unit =
      let radius = 0. in
      let width = float_of_int self#offset_width in
      let height = float_of_int self#offset_height in
      let corner_width = radius +. 1.2 in
      let leading_stroke_length = Float.abs (12. -. corner_width) in
      (* If the notchWidth is 0, the the notched outline
       *  doesn't need to add padding. *)
      let padded_notch_width =
        if Float.(notch_width > 0.) then notch_width +. 8. else 0. in
      let ( & ) s f = s ^ (Printf.sprintf "%g" f) in
      let ( ! ), ( + ), ( - ), ( * ) = Float.(neg, add, ( - ), ( * )) in
      let r = radius in
      let cw = 2. * corner_width in
      let path_middle =
        ("a" & r) ^ ("," & r) ^ (" 0 0 1 " & r) ^ ("," & r)
        ^ ("v" & (height - cw))
        ^ ("a" & r) ^ ("," & r) ^ (" 0 0 1 " & !r) ^ ("," & r)
        ^ ("h" & (!width + cw))
        ^ ("a" & r) ^ ("," & r) ^ (" 0 0 1 " & !r) ^ ("," & !r)
        ^ ("v" & (!height + cw))
        ^ ("a" & r) ^ ("," & r) ^ (" 0 0 1 " & r) ^ ("," & !r) in
      let path_string =
        ("M" & (corner_width + leading_stroke_length + padded_notch_width))
        ^ ("," & 1.)
        ^ ("h" & (width - cw - padded_notch_width - leading_stroke_length))
        ^ path_middle
        ^ ("h" & leading_stroke_length) in
      let (elt : Dom_html.element Js.t) = Js.Unsafe.coerce (Svg.toelt path) in
      elt##setAttribute (Js.string "d") (Js.string path_string)

  end
