open Widget
open Tyxml_js

module Row = struct

  module Section = struct

    module Title = struct
      class t ~title () = object
        inherit [Dom_html.element Js.t] widget (Markup.Toolbar.Row.Section.create_title ~title ()
                                                |> To_dom.of_element) () as super

        method title       = super#text_content
        method set_title s = super#set_text_content s
      end
    end

    class t ~content () =

      let elt =
        Markup.Toolbar.Row.Section.create ~content:(List.map (fun x -> Of_dom.of_element x#element) content) ()
        |> To_dom.of_section in

      object

        val mutable align : [ `Start | `End | `Center ] = `Center

        inherit [Dom_html.element Js.t] widget elt () as super

        method align        = align
        method remove_align = (match align with
                               | `Start  -> super#remove_class Markup.Toolbar.Row.Section.align_start_class
                               | `End    -> super#remove_class Markup.Toolbar.Row.Section.align_end_class
                               | `Center -> ());
                              align <- `Center;
        method set_align x  = (match x with
                               | `Start  -> super#add_class Markup.Toolbar.Row.Section.align_start_class
                               | `End    -> super#add_class Markup.Toolbar.Row.Section.align_end_class
                               | `Center -> ());
                              align <- x

        method shrink_to_fit     = super#add_class Markup.Toolbar.Row.Section.shrink_to_fit_class
        method not_shrink_to_fit = super#remove_class Markup.Toolbar.Row.Section.shrink_to_fit_class
      end

  end

  class t ~sections () =
    let elt = Markup.Toolbar.Row.create ~content:(List.map (fun x -> Of_dom.of_element x#element) sections) ()
              |> To_dom.of_div in

    object

      inherit [Dom_html.divElement Js.t] widget elt ()

    end

end

class type mdc =
  object
    method fixedAdjustElement : Dom_html.element Js.t Js.prop
  end

class type change_event =
  object
    inherit Dom_html.event
    method detail_ : < flexibleExpansionRatio : Js.number Js.t Js.readonly_prop > Js.t Js.readonly_prop
  end

type events =
  { change : change_event Js.t Dom_events.Typ.typ
  }

let events =
  { change = Dom_events.Typ.make "MDCToolbar:change"
  }

class t ~rows () =

  let elt = Markup.Toolbar.create ~content:(List.map (fun x -> Of_dom.of_element x#element) rows)
                                  ()
            |> To_dom.of_header in

  object

    inherit [Dom_html.element Js.t] widget elt ()

    val mdc : mdc Js.t = Js.Unsafe.global##.mdc##.toolbar##.MDCToolbar##attachTo elt

  end
