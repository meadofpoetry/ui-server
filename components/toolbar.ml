open Containers
open Tyxml_js

module Markup = Components_markup.Toolbar.Make(Xml)(Svg)(Html)

module Row = struct

  module Section = struct

    module Title = struct
      class t ~title () = object
        inherit Widget.t (Markup.Row.Section.create_title ~title ()
                               |> Tyxml_js.To_dom.of_element) () as super

        method title       = super#text_content |> Option.get_or ~default:""
        method set_title s = super#set_text_content s
      end
    end

    class t ?(align=`Center) ~(widgets:#Widget.t list) () =

      let elt =
        Markup.Row.Section.create ~content:(List.map Widget.to_markup widgets) ()
        |> Tyxml_js.To_dom.of_section in

      object(self)

        val mutable align : [ `Start | `End | `Center ] = align
        val mutable widgets : Widget.t list = List.map (fun x -> (x :> Widget.t)) widgets

        inherit Widget.t elt () as super

        method widgets = widgets

        method align        = align
        method set_align x  =
          self#remove_align;
          align <- x;
          match x with
          | `Start  -> super#add_class Markup.Row.Section.align_start_class
          | `End    -> super#add_class Markup.Row.Section.align_end_class
          | `Center -> ()

        method set_shrink_to_fit x =
          Markup.Row.Section.shrink_to_fit_class
          |> (fun c -> if x then super#add_class c else super#remove_class c)

        method private remove_align =
          align <- `Center;
          match align with
          | `Start  -> super#remove_class Markup.Row.Section.align_start_class
          | `End    -> super#remove_class Markup.Row.Section.align_end_class
          | `Center -> ()

        initializer
          self#set_align align
      end

  end

  class t ~(sections:Section.t list) () =
    let elt = Markup.Row.create ~content:(List.map Widget.to_markup sections) ()
              |> Tyxml_js.To_dom.of_div in
    object
      inherit Widget.t elt ()
      method sections = sections
    end

end

(* TODO remove *)
class type mdc =
  object
    method fixedAdjustElement : Dom_html.element Js.t Js.prop
  end

class type change_event =
  object
    inherit Dom_html.event
    method detail : < flexibleExpansionRatio : float Js.readonly_prop > Js.t Js.readonly_prop
  end

type events =
  { change : change_event Js.t Dom_events.Typ.typ
  }

let events =
  { change = Dom_events.Typ.make "MDCToolbar:change"
  }

class t ~rows () =
  let elt = Markup.create ~content:(List.map Widget.to_markup rows) () |> Tyxml_js.To_dom.of_header in
  object
    inherit Widget.t elt ()
    val mdc : mdc Js.t = Js.Unsafe.global##.mdc##.toolbar##.MDCToolbar##attachTo elt
  end
