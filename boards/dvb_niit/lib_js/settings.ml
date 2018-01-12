open Board_types
open Components

let module_settings ~(init  : settings)
                    ~(event : settings React.event) =
  let mode_items = [ new Select.Base.Item.t ~text:"DVB-T2" ~value:T2 ()
                   ; new Select.Base.Item.t ~text:"DVB-T"  ~value:T  ()
                   ; new Select.Base.Item.t ~text:"DVB-C"  ~value:C  ()
                   ]
  in
  let mode       = new Select.Base.t ~label:"Стандарт" ~items:mode_items () in
  let bw_items   = [ new Select.Base.Item.t ~text:"8 МГц" ~value:Bw8 ()
                   ; new Select.Base.Item.t ~text:"7 МГц" ~value:Bw7 ()
                   ; new Select.Base.Item.t ~text:"6 МГц" ~value:Bw6 ()
                   ]
  in
  let bw         = new Select.Base.t ~label:"Полоса пропускания" ~items:bw_items () in
  let box        = new Box.t ~widgets:[ mode#widget; bw#widget ] () in
  let _ = mode#select_value init.mode in
  let _ = bw#select_value init.bw in
  let s = React.S.l2 (fun mode bw -> match mode, bw with
                                     | Some mode, Some bw -> Ok { mode; bw; freq = 586000000l; plp = 0 }
                                     | _                  -> Error "some values missing")
                     mode#s_selected bw#s_selected
  in
  box,s

let card control
         ~(init  : config)
         ~(event : config React.event) =
  let open Tabs in
  let tabs =
    CCList.map (fun (id,s) ->
        let page,_ = module_settings ~init:s ~event:(React.E.map (fun x -> CCList.Assoc.get_exn id x) event) in
        { href    = None
        ; content  = `Text (Printf.sprintf "Модуль %d" (succ id))
        ; disabled = false
        ; value    = page })
               (CCList.sort (fun (id1,_) (id2,_) -> compare id1 id2) init)
  in
  let bar      = new Tabs.Tab_bar.t ~tabs () in
  let title    = new Card.Title.t ~title:"Настройки" () in
  let primary  = new Card.Primary.t ~widgets:[title] () in
  let tab_sect = new Card.Actions.t ~widgets:[bar] () in
  let settings = Dom_html.createDiv Dom_html.document in
  let content  = new Card.Media.t ~widgets:[Widget.create settings] () in
  let card     = new Card.t ~sections:[ `Primary primary
                                      ; `Actions tab_sect
                                      ; `Media content
                                      ] ()
  in
  let _ = React.S.map (function
                       | Some tab -> Dom.list_of_nodeList @@ settings##.childNodes
                                     |> CCList.iter (fun x -> Dom.removeChild settings x);
                                     Dom.appendChild settings tab#get_value#root
                       | None     -> ()) bar#s_active
  in
  title#add_class "color--primary-on-primary";
  primary#add_class "background--primary";
  tab_sect#style##.padding := Js.string "0";
  card

let layout control
           ~(init  : config)
           ~(event : config React.event) =
  let card = card control ~init ~event in
  let cell = new Layout_grid.Cell.t ~widgets:[card] () in
  let grid = new Layout_grid.t ~cells:[cell] () in
  grid

let page control =
  let open Lwt_result.Infix in
  let div = Dom_html.createDiv Dom_html.document in
  let t   =
    Requests.get_config control
    >>= (fun init ->
      let event,sock = Requests.get_config_ws control in
      let grid = layout control ~init ~event in
      Dom.appendChild div grid#root;
      Lwt_result.return sock)
  in
  div,(fun () -> t >>= (fun x -> x##close; Lwt_result.return ()) |> ignore)
