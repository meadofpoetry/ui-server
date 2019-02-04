open Components

let base_class = "topology__drawer"

let make_header text =
  let icon = Icon.SVG.(create_simple Path.close) in
  let close = new Icon_button.t ~icon () in
  let font = Typography.Headline_5 in
  let title = new Typography.Text.t ~adjust_margin:false ~font ~text () in
  let box = new Hbox.t ~widgets:[title#widget; close#widget] () in
  box#add_class @@ CSS.add_element base_class "header";
  title#add_class @@ CSS.add_element base_class "title";
  close#add_class @@ CSS.add_element base_class "close";
  box, close, title#set_text

let make ~title () =
  let header, close, set_title = make_header title in
  let divider = new Divider.t () in
  let box = new Vbox.t ~widgets:[] () in
  let content = [header#widget; divider#widget; box#widget] in
  let drawer = Side_sheet.make ~content () in
  close#listen_lwt' Widget.Event.click (fun _ _ ->
      drawer#hide (); Lwt.return_unit);
  drawer#add_class base_class;
  box#add_class @@ CSS.add_element base_class "body";
  drawer, box, set_title
