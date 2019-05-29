open Components

let base_class = "topology__drawer"

let make_header ~title () =
  let close = Icon_button.make ~icon:Icon.SVG.(make_simple Path.close) () in
  let title = Typography.Text.make ~font:Headline_5 title in
  let box = Box.make ~dir:`Row [title#widget;close#widget] in
  box#add_class @@ BEM.add_element base_class "header";
  title#add_class @@ BEM.add_element base_class "title";
  close#add_class @@ BEM.add_element base_class "close";
  box, close, title#set_text

let make ?(anchor=`Right) ~title () =
  let header, close, set_title = make_header ~title () in
  let divider = Divider.make () in
  let box = Box.make
      ~dir:(match anchor with
          | `Bottom | `Top -> `Row
          | `Left | `Right -> `Column)
      [] in
  let content = [header#widget; divider#widget; box#widget] in
  let drawer = Side_sheet.make content () in
  let clicks = Events.clicks close#root (fun _ _ ->
      drawer#toggle ~force:false ()) in
  drawer#set_on_destroy (fun () -> Lwt.cancel clicks);
  drawer#add_class base_class;
  box#add_class @@ BEM.add_element base_class "body";
  drawer, box, set_title
