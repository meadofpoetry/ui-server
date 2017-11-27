module Row = struct

  module Section = struct

    module Title = struct
      class t ~title () = object
        inherit Widget.widget (Markup.Toolbar.Row.Section.create_title ~title ()
                               |> Tyxml_js.To_dom.of_element) () as super

        method title       = super#text_content
        method set_title s = super#set_text_content s
      end
    end

    class t ~(widgets:#Widget.widget list) () =

      let elt =
        Markup.Toolbar.Row.Section.create ~content:(Widget.widgets_to_markup widgets) ()
        |> Tyxml_js.To_dom.of_section in

      object

        val mutable align : [ `Start | `End | `Center ] = `Center
        val mutable widgets : Widget.widget list = List.map (fun x -> (x :> Widget.widget)) widgets

        inherit Widget.widget elt () as super

        method widgets = widgets

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
    let elt = Markup.Toolbar.Row.create ~content:(Widget.widgets_to_markup sections) ()
              |> Tyxml_js.To_dom.of_div in
    object

      inherit Widget.widget elt ()

    end

end

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

  let elt = Markup.Toolbar.create ~content:(Widget.widgets_to_markup rows) () |> Tyxml_js.To_dom.of_header in
  object
    inherit Widget.widget elt ()
    val mdc : mdc Js.t = Js.Unsafe.global##.mdc##.toolbar##.MDCToolbar##attachTo elt
  end
