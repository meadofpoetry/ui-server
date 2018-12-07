open Js_of_ocaml
open Containers
open Tyxml_js

module Markup = Components_markup.Tab.Make(Xml)(Svg)(Html)

type _ content =
  | Text : string -> Widget.t content
  | Icon : (#Widget.t as 'a) -> 'a content
  | Both : string * (#Widget.t as 'a) -> (Widget.t * 'a) content

type dimensions =
  { root_left : int
  ; root_right : int
  ; content_left : int
  ; content_right : int
  }

let interacted_event = "MDCTab:interacted"

let content_to_elt : type a. a content ->
                          Tab_indicator.t ->
                          'c Html.elt * a =
  fun content indicator ->
  let indicator = Widget.to_markup indicator in
  match content with
  | Text s ->
     let text =
       Markup.create_text_label s ()
       |> To_dom.of_element
       |> Widget.create in
     let content = Markup.create_content [Widget.to_markup text] () in
     Markup.create content indicator (), text
  | Icon icon ->
     icon#add_class Markup.icon_class;
     let content = Markup.create_content [Widget.to_markup icon] () in
     Markup.create content indicator (), icon
  | Both (text, icon) ->
     icon#add_class Markup.icon_class;
     let text =
       Markup.create_text_label text ()
       |> To_dom.of_element
       |> Widget.create in
     let content = Markup.create_content [ Widget.to_markup icon
                                         ; Widget.to_markup text ] () in
     Markup.create content indicator (), (text, icon)

class ['a, 'b] t
        ?(min_width = false)
        ?(disabled = false)
        ?(active = false)
        ~(value : 'b)
        ~(content : 'a content)
        () =
  let indicator = new Tab_indicator.t () in
  let elt, _ = content_to_elt content indicator in
  let elt = To_dom.of_element elt in

  object(self : 'self)

    inherit Widget.t elt () as super

    val mutable _ripple : Ripple.t option = None
    val mutable _value : 'b = value
    val mutable _click_listener = None

    method! init () : unit =
      super#init ();
      if min_width then self#add_class Markup.min_width_class;
      self#set_active active;
      self#set_disabled disabled;
      self#listen Widget.Event.click (fun _ _ ->
          self#emit ~should_bubble:true
            interacted_event
            (Js.Unsafe.inject self#root);
          false)
      |> (fun x -> _click_listener <- Some x);
      let ripple_surface = self#ripple_element in
      let adapter = Ripple.make_default_adapter (self :> Widget.t) in
      let add_class = fun s ->
        ripple_surface##.classList##add (Js.string s) in
      let remove_class = fun s ->
        ripple_surface##.classList##remove (Js.string s) in
      let update_css_variable = fun name value ->
        Ripple.update_css_variable ripple_surface name value in
      let is_surface_disabled = fun () ->
        self#disabled in
      let adapter =
        { adapter with add_class
                     ; remove_class
                     ; update_css_variable
                     ; is_surface_disabled } in
      let ripple = new Ripple.t adapter () in
      _ripple <- Some ripple

    method! layout () : unit =
      super#layout ();
      Option.iter (fun r -> r#layout ()) _ripple

    method! destroy () : unit =
      super#destroy ();
      Option.iter Dom_events.stop_listen _click_listener;
      _click_listener <- None;
      Option.iter (fun r -> r#destroy ()) _ripple;
      _ripple <- None

    method indicator : Tab_indicator.t =
      indicator

    method value : 'b =
      _value

    method set_value (x : 'b) : unit =
      _value <- x

    method content : 'a content =
      content

    method disabled : bool =
      self#has_attribute "disabled"

    method set_disabled (x : bool) : unit =
      let a = "disabled" in
      if x then self#set_attribute a "true"
      else self#remove_attribute a

    method focus () : unit =
      self#root##focus

    method active : bool =
      self#has_class Markup.active_class

    method set_active ?(previous : 'self option) (x : bool) : unit =
      self#add_or_remove_class x Markup.active_class;
      self#set_attribute "aria-selected" @@ string_of_bool x;
      self#set_attribute "tabindex" (if x then "0" else "-1");
      let prev_indicator = Option.map (fun x -> x#indicator) previous in
      indicator#set_active ?previous:prev_indicator x;
      if x then self#focus ()

    method compute_dimensions () : dimensions =
      let root_width = self#offset_width in
      let root_left = self#offset_left in
      let content_width = self#content_element##.offsetWidth in
      let content_left = self#content_element##.offsetLeft in
      { root_left
      ; root_right = root_left + root_width
      ; content_left = root_left + content_left
      ; content_right = root_left + content_left + content_width
      }

    method index : int =
      let rec aux i node =
        match Js.Opt.to_option node##.previousSibling with
        | None -> i
        | Some x -> aux (succ i) x in
      aux 0 self#node

    method width : int =
      self#offset_width

    method left : int =
      self#offset_left

    (* Private methods *)

    method private ripple_element =
      Option.get_exn @@ self#get_child_element_by_class Markup.ripple_class

    method private content_element =
      Option.get_exn @@ self#get_child_element_by_class Markup.content_class

  end
