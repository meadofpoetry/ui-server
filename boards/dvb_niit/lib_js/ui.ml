open Containers
open Components

let card ~(title    : string)
         ~(tab_bar  : #Widget.widget Tabs.Tab_bar.t)
         ~(sections : Card.sections) =
  let title    = new Card.Title.t ~title () in
  let primary  = new Card.Primary.t ~widgets:[title] () in
  let tab_sect = new Card.Actions.t ~widgets:[tab_bar] () in
  let inner    = Dom_html.createDiv Dom_html.document in
  let content  = new Card.Media.t ~widgets:[Widget.create inner] () in
  let card     = new Card.t ~sections:([ `Primary primary
                                       ; `Actions tab_sect
                                       ; `Media content
                                       ] @ sections) ()
  in
  let _ = React.S.map (function
                       | Some tab -> Dom.list_of_nodeList @@ inner##.childNodes
                                     |> List.iter (fun x -> Dom.removeChild inner x);
                                     Dom.appendChild inner tab#get_value#root
                       | None     -> ()) tab_bar#s_active
  in
  title#add_class "color--primary-on-primary";
  primary#add_class "background--primary";
  tab_sect#style##.padding := Js.string "0";
  card
