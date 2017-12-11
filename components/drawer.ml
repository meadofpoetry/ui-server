module Permanent = struct

  include Markup.Drawer.Permanent

  class type t = Dom_html.element

end

module Persistent = struct

  include Markup.Drawer.Persistent

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

  include Markup.Drawer.Temporary

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
