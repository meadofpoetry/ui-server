open Components

let section () =
  let tabs =
    List.map (fun label ->
        let indicator = Tab_indicator.make () in
        Tab.make ~label ~indicator ())
      ["first"; "second"; "third"] in
  let scroller = Tab_scroller.make tabs in
  let bar = Tab_bar.make ~auto_activation:true scroller in
  bar
