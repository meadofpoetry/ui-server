open Widget
module Widgets = Markup

module Avatar          = Avatar
module Widget          = Widget
module Button          = Button
module Checkbox        = Checkbox
module Dialog          = Dialog
module Elevation       = Elevation
module Fab             = Fab
module Form_field      = Form_field
module Grid_list       = Grid_list
module Icon            = Icon
module Icon_toggle     = Icon_toggle
module Layout_grid     = Layout_grid
module Linear_progress = Linear_progress
module List_           = List_
module Menu            = Menu
module Radio           = Radio
module Ripple          = Ripple
module Select          = Select
module Slider          = Slider
module Snackbar        = Snackbar
module Switch          = Switch
module Tabs            = Tabs
module Textfield       = Textfield
module Textarea        = Textarea
module Tree            = Tree
module Typography      = Typography

module Card = struct

  include Widgets.Card

  class type t = Dom_html.divElement

  let attach elt : t Js.t = Tyxml_js.To_dom.of_div elt

end

module Drawer = struct

  module Permanent = struct

    include Widgets.Drawer.Permanent

    class type t = Dom_html.element

  end

  module Persistent = struct

    include Widgets.Drawer.Persistent

    class type t =
      object
        method root__ : Dom_html.element Js.t Js.readonly_prop
        method open_  : bool Js.t Js.prop
      end

    type events =
      { opened : Dom_html.event Js.t Dom_events.Typ.typ
      ; closed : Dom_html.event Js.t Dom_events.Typ.typ
      }

    let events =
      { opened = Dom_events.Typ.make "MDCPersistentDrawer:open"
      ; closed = Dom_events.Typ.make "MDCPersistentDrawer:close"
      }

    let attach (elt : Html_types.aside Tyxml_js.Html.elt) : t Js.t =
      Js.Unsafe.global##.mdc##.drawer##.MDCPersistentDrawer##attachTo elt

  end

  module Temporary = struct

    include Widgets.Drawer.Temporary

    class type t =
      object
        method root__ : Dom_html.element Js.t Js.readonly_prop
        method open_  : bool Js.t Js.prop
      end

    type events =
      { opened : Dom_html.event Js.t Dom_events.Typ.typ
      ; closed : Dom_html.event Js.t Dom_events.Typ.typ
      }

    let events =
      { opened = Dom_events.Typ.make "MDCTemporaryDrawer:open"
      ; closed = Dom_events.Typ.make "MDCTemporaryDrawer:close"
      }

    let attach (elt : Html_types.aside Tyxml_js.Html.elt) : t Js.t =
      Js.Unsafe.global##.mdc##.drawer##.MDCTemporaryDrawer##attachTo elt

  end

end

module Toolbar = struct

  include Widgets.Toolbar

  class type t =
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

  let attach elt : t Js.t =
    Js.Unsafe.global##.mdc##.toolbar##.MDCToolbar##attachTo elt

end
