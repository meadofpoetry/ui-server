open Js_of_ocaml
open Components

let ( >>= ) = Lwt.bind

let section () =
  let actions =
    [ Dialog.Markup.create_action
        ~action:Accept
        ~label:"accept"
        ()
    ; Dialog.Markup.create_action
        ~action:Close
        ~label:"cancel"
        ~default:true
        ()
    ; Dialog.Markup.create_action
        ~label:"Don't know"
        ()
    ] in
  let title =
    Dialog.Markup.create_title_simple
      ~title:"This is a simple dialog"
      () in
  let content =
    Dialog.Markup.create_content_simple
      {|Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed do eiusmod tempor incididunt ut labore et dolore magna aliqua. Adipiscing elit duis tristique sollicitudin nibh sit. Enim ut tellus elementum sagittis. Nulla facilisi etiam dignissim diam quis. Morbi non arcu risus quis varius. Bibendum enim facilisis gravida neque convallis. Lacus luctus accumsan tortor posuere. Elit duis tristique sollicitudin nibh sit amet commodo nulla facilisi. Sit amet est placerat in egestas erat imperdiet sed euismod. Risus nullam eget felis eget nunc lobortis mattis aliquam faucibus. Risus quis varius quam quisque id diam. Sapien nec sagittis aliquam malesuada bibendum. Orci ac auctor augue mauris augue. Egestas fringilla phasellus faucibus scelerisque eleifend donec pretium vulputate sapien. Est pellentesque elit ullamcorper dignissim. Dictum varius duis at consectetur lorem donec. Id aliquet risus feugiat in ante metus dictum at. Urna nunc id cursus metus. Eget gravida cum sociis natoque penatibus et magnis dis parturient.|}
      () in
  let dialog = Dialog.make ~title ~content ~actions () in
  let button =
    Button.make
      ~label:"show"
      ~on_click:(fun _ _ ->
        dialog#open_await ()
        >>= fun x -> print_endline (Dialog.action_to_string x);
                     Lwt.return_unit)
      () in
  Dom.appendChild Dom_html.document##.body dialog#root;
  Widget.create_div ~widgets:[button] ()
