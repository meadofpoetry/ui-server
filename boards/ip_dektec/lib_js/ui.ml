open Containers
open Components

module Row = struct

  open Tyxml_js.Html

  class t ?(icon=false) ~label ~e () =
    let nw = span [pcdata label] |> Tyxml_js.To_dom.of_element |> Widget.create in
    let vw = if icon
             then new Icon.Font.t ~icon:"" () |> Widget.coerce
             else span [pcdata "-"] |> Tyxml_js.To_dom.of_element |> Widget.create in
    object
      inherit Box.t ~vertical:false ~widgets:[Widget.coerce nw; Widget.coerce vw] () as super
      method has_icon = icon
      method get_value_widget = vw
      method get_label_widget = nw
      initializer
        Typography.set ~font:Subheading_1 nw;
        Typography.set ~font:Body_2 vw;
        React.E.map (fun s -> vw#set_text_content s) e |> ignore;
        super#set_justify_content `Space_between
    end

end

module Rows = struct

  class t ~(rows:Row.t list) ~s_state () = object
    inherit Box.t ~widgets:rows ()
    initializer
      React.S.map (fun x -> if not x
                            then List.iter (fun row -> let s = if row#has_icon then "" else "-" in
                                                       row#get_value_widget#set_text_content s)
                                   rows) s_state |> ignore;
  end

end

module Stateful_card = struct

  class t ?form ?subtitle ~title ~sections ~s_state () = object
    inherit Card.t ?form ~widgets:sections () as super
    initializer
      super#style##.height := Js.string "100%";
        s_state |> ignore
  end

end

module Settings_card = struct

  class t ?subtitle ~title ~sections ~s_state ~f_submit () =
    let apply_btn = new Button.t ~compact:true ~label:"Применить" ~typ:`Submit () in
    object
      inherit Stateful_card.t ~form:true
                ?subtitle
                ~title
                ~s_state
                ~sections:(sections @ [(new Card.Actions.t ~widgets:[apply_btn] ())#widget ])
                ()
      method get_apply_button = apply_btn
      initializer
        React.E.map (fun _ -> (apply_btn#set_disabled true;
                               let open Lwt.Infix in
                               f_submit ()
                               >|= (fun _ -> apply_btn#set_disabled false) |> ignore))
          apply_btn#e_click |> ignore;
        React.S.map (fun x  -> apply_btn#set_disabled @@ not x) s_state |> ignore;
    end

end
