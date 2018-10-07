open Containers
open Tyxml_js

module Markup = Components_markup.Tab.Make(Xml)(Svg)(Html)

type _ content =
  | Text : string -> Widget.t content
  | Icon : (#Widget.t as 'a) -> 'a content
  | Both : string * (#Widget.t as 'a) -> (Widget.t * 'a) content

let content_to_elt : type a. a content ->
                          Tab_indicator.t ->
                          'c Html.elt * a =
  fun content indicator ->
  let indicator = Widget.to_markup indicator in
  match content with
  | Text s ->
     let text = Markup.create_text_label s ()
                |> To_dom.of_element
                |> Widget.create in
     let content = Markup.create_content [ Widget.to_markup text ] () in
     Markup.create content indicator (), text
  | Icon icon ->
     icon#add_class Markup.icon_class;
     let content = Markup.create_content [ Widget.to_markup icon ] () in
     Markup.create content indicator (), icon
  | Both (text, icon) ->
     icon#add_class Markup.icon_class;
     let text = Markup.create_text_label text ()
                |> To_dom.of_element
                |> Widget.create in
     let content = Markup.create_content [ Widget.to_markup icon
                                         ; Widget.to_markup text ] () in
     Markup.create content indicator (), (text, icon)

class ['a,'b] t
        ?(min_width = false)
        ?(disabled = false)
        ?(active = false)
        ~(value : 'b)
        ~(content : 'a content)
        () =

  let indicator = new Tab_indicator.t () in
  let elt, elts = content_to_elt content indicator in
  let elt = To_dom.of_element elt in

  object(self : 'self)

    inherit Widget.t elt () as super

    val mutable _value : 'b = value

    method indicator = indicator

    method value : 'b       = _value
    method set_value (x:'b) = _value <- x

    method content : 'a content = content

    method disabled : bool =
      self#has_attribute "disabled"
    method set_disabled (x:bool) : unit =
      let a = "disabled" in
      if x then self#set_attribute a "true"
      else self#remove_attribute a

    method active : bool =
      self#has_class Markup.active_class
    method set_active ?(previous:'self option) (x:bool) : unit =
      let prev_indicator = Option.map (fun x -> x#indicator) previous in
      indicator#set_active ?previous:prev_indicator x;
      self#set_attribute "aria-selected" @@ string_of_bool x;
      self#set_attribute "tabindex" (if x then "0" else "-1");
      self#add_or_remove_class x Markup.active_class

    method width = self#offset_width
    method left  = self#offset_left

    initializer
      if min_width then self#add_class Markup.min_width_class;
      self#set_active active;
      self#set_disabled disabled;

  end
