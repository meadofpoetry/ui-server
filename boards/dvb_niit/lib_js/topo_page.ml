open Containers
open Components

let make ~make_tabs (board:Common.Topology.topo_board) =
  let open Lwt_result.Infix in
  let listener = Dvb_niit.listen board.control in
  let box      = Dom_html.createDiv Dom_html.document |> Widget.create in
  listener
  >>= (fun l ->
    let pages =
      List.map (fun (id,init) ->
          let event = React.E.map (fun x -> List.Assoc.get_exn ~eq:(=) id x) l.events.config in
          let b,s,submit = Settings.make_module_settings ~id ~init ~event ~state:l.state board.control () in
          let apply   = new Button.t ~label:"Применить" () in
          let _       = React.E.map (fun _ -> Option.iter (fun s -> submit s |> ignore) @@ React.S.value s)
                                    apply#e_click
          in
          let actions = new Card.Actions.t ~widgets:[apply#widget] () in
          let w = new Box.t ~vertical:true ~widgets:[b#widget;actions#widget] () in
          (* let w = new Expansion_panel.t
           *             ~elevation:0
           *             ~actions:[apply#widget]
           *             ~title:(Printf.sprintf "Модуль %d" id)
           *             ~content:[b#widget]
           *             () in *)
          Printf.sprintf "Модуль %d" (succ id), w#widget)
               (List.sort (fun (id1,_) (id2,_) -> compare id1 id2) l.config)
    in
    (* (match List.head_opt pages with
     *  | Some w -> w#set_expanded true
     *  | None   -> ()); *)
    (* let b = Dom_html.createDiv Dom_html.document |> Widget.create in
     * List.iter (fun x -> Dom.appendChild b#root x#root) pages; *)
    let bar = make_tabs pages in
    Dom.appendChild box#root bar#root;
    Lwt_result.return ()) |> ignore;
  box#widget, (fun () -> Dvb_niit.unlisten listener |> ignore)
