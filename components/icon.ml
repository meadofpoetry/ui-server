open Containers

module Font = struct

  class t ~icon () =
    let elt = Markup.Icon.Font.create ~icon () |> Tyxml_js.To_dom.of_i in
    let e_click,e_click_push = React.E.create () in
    object(self)
      inherit Widget.widget elt () as super
      method e_click    = e_click
      method get_icon   = super#get_text_content |> Option.get_or ~default:""
      method set_icon i = super#set_text_content i

      initializer
        Dom_events.listen self#root Dom_events.Typ.click (fun _ e -> e_click_push e; true) |> ignore
    end

end

module Button = struct

  module Font = struct

    class t ~icon () =

    object(self)
      inherit Font.t ~icon ()
      initializer
        self#add_class Markup.Icon.button_class;
        (let r = Ripple.attach self in
         self#add_class "mdc-ripple-surface"; (*FIXME*)
         r##.unbounded := Js.bool true;
         Ripple.set_unbounded self)
    end

  end

end
