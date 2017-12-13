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

  module Primary = struct

    class t ?subtitle ~title () =
      let title_widget    = new Card.Title.t ~title () in
      let subtitle_widget = CCOpt.map (fun x -> new Card.Subtitle.t ~subtitle:x ()) subtitle in
      object
        inherit Card.Primary.t ~widgets:([]
                                         |> CCList.cons_maybe @@ CCOpt.map Widget.coerce subtitle_widget
                                         |> CCList.cons @@ Widget.coerce title_widget)
                               () as super
        initializer
          title_widget#add_class "color--primary-on-primary";
          super#add_class "background--primary";
      end

  end

  class t ?form ?subtitle ~title ~sections ~s_state () = object
    inherit Card.t ?form ~sections:(CCList.cons (`Primary (new Primary.t ?subtitle ~title ()))
                                                sections)
                   () as super
    initializer
      super#style##.height := Js.string "100%";
      CCOpt.iter (fun x -> x#style##.height := Js.string "100%";
                           (Js.Unsafe.coerce x#style)##.justifyContent := Js.string "flex-start") super#get_media;
      React.S.map (function
                   | false -> super#style##.backgroundColor := Js.string "#ef9a9a";
                              CCList.iter (function
                                           | `Media x -> x#add_class "color--disabled-on-background"
                                           | _ -> ()) sections;
                   | true  -> super#style##.backgroundColor := Js.string "";
                              CCList.iter (function
                                           | `Media x -> x#remove_class "color--disabled-on-background"
                                           | _ -> ()) sections)
                  s_state |> ignore
  end

end

module Settings_card = struct

  class t ?subtitle ~title ~sections ~s_state ~f_submit ~s_valid () =
    let apply_btn = new Button.t ~compact:true ~label:"Применить" ~typ:`Submit () in
    object
      inherit Stateful_card.t ~form:true
                              ?subtitle
                              ~title
                              ~s_state
                              ~sections:(sections @ [ `Actions (new Card.Actions.t ~widgets:[apply_btn] ()) ])
                              ()
      method get_apply_button = apply_btn
      initializer
        React.E.map (fun () -> if React.S.value s_valid
                               then (apply_btn#set_disabled true;
                                     let open Lwt.Infix in
                                     f_submit ()
                                     >|= (fun _ -> apply_btn#set_disabled false) |> ignore))
                    apply_btn#e_click |> ignore;
        React.S.map (fun x  -> apply_btn#set_disabled @@ not x) s_state |> ignore;
    end

end
