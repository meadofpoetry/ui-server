open Js_of_ocaml_tyxml
open Components

let section () =
  let cc = Textfield.Character_counter.make () in
  let ht = Textfield.Helper_text.make
      ~validation:true
      "Invalid e-mail" in
  let textfield =
    Textfield.make_textfield
      ~leading_icon:(Icon.SVG.(make_simple Path.heart))
      ~trailing_icon:(Icon.SVG.(make_simple Path.airplane))
      ~max_length:18
      ~value:"initial text"
      ~helper_text:ht
      ~character_counter:cc
      ~label:"Label"
      Email in
  let hl = Widget.create
    @@ Tyxml_js.To_dom.of_element
    @@ Textfield.Markup.create_helper_line
      [ Widget.to_markup ht
      ; Widget.to_markup cc ] () in
  let cc = Textfield.Character_counter.make () in
  let textarea =
    Textfield.make_textarea
      ~max_length:22
      ~character_counter:cc
      ~label:"Label" () in
  let div =
    Widget.create_div
      ~widgets:[ textfield#widget
               ; hl
               ; textarea#widget ]
      () in
  div#root##.style##.width := Utils.px_js 250;
  div
