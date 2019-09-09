open Components

let base_class = "topology__drawer"

let make_header ~title () =
  let icon = Icon.SVG.(Markup_js.create ~d:Path.close ()) in
  let close = Icon_button.make ~classes:[BEM.add_element base_class "close"] ~icon () in
  let title =
    Typography.Text.make
      ~classes:[BEM.add_element base_class "title"]
      ~font:Headline_5
      ~text:title
      ()
  in
  let box =
    Box.make
      ~classes:[BEM.add_element base_class "header"]
      ~children:[title#markup; close#markup]
      ()
  in
  box#set_on_destroy (fun () ->
      title#destroy ();
      close#destroy ());
  box, close, title#set_text

let make ?(anchor = `Right) ~title () =
  let header, close, set_title = make_header ~title () in
  let divider = Divider.Markup_js.create_hr () in
  let box =
    Box.Markup_js.create
      ~classes:[BEM.add_element base_class "body"]
      ~vertical:
        (match anchor with
        | `Bottom | `Top -> false
        | `Left | `Right -> true)
      ()
  in
  let children = [header#markup; divider; box] in
  let drawer = Side_sheet.make ~classes:[base_class] ~children () in
  let clicks =
    Js_of_ocaml_lwt.Lwt_js_events.clicks close#root (fun _ _ ->
        drawer#toggle ~force:false ())
  in
  drawer#set_on_destroy (fun () ->
      header#destroy ();
      Lwt.cancel clicks);
  drawer, box, set_title
