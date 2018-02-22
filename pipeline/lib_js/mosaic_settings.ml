open Containers
open Components
open Requests
open Lwt_result.Infix

module Wm = struct

  let make_layout (wm: Wm.t) =
    Layout.initialize wm

  let make_toolbar ~start_actions ~end_actions =
    let start_section = new Toolbar.Row.Section.t ~align:`Start ~widgets:start_actions () in
    let end_section   = new Toolbar.Row.Section.t ~align:`End ~widgets:end_actions () in
    let sections = [start_section; end_section] in
    let row      = new Toolbar.Row.t ~sections () in
    let tb       = new Toolbar.t ~rows:[row] () in
    let ()       = tb#add_class "wm-toolbar" in
    tb

  let make_placeholder ~text ~icon () =
    let ph = Dom_html.createDiv Dom_html.document |> Widget.create in
    ph#add_class "wm-grid-placeholder";
    let txt = new Typography.Text.t ~text () in
    let ico = new Icon.Font.t ~icon () in
    let box = new Box.t ~widgets:[txt#widget;ico#widget] () in
    let _   = box#set_align_items `Center in
    let _   = box#set_justify_content `Center in
    Dom.appendChild ph#root box#root;
    ph

  let create
        ~(init:   Wm.t)
        ~(events: Wm.t React.event)
        ~(post:   Wm.t -> unit) =
    let open Layout in
    let id   = "wm-widget" in
    let div  = Dom_html.createDiv Dom_html.document in
    let cell = new Layout_grid.Cell.t ~widgets:[Widget.create div] () in
    cell#set_span 12;
    let grid = new Layout_grid.t ~cells:[cell] () in
    let make (wm : Wm.t) =
      let grid,layout,f_add,f_rm = make_layout wm in
      let add     = new Icon.Button.Font.t ~icon:"add" () in
      let rm      = new Icon.Button.Font.t ~icon:"delete" () in
      let edit    = new Icon.Button.Font.t ~icon:"edit" () in
      let apply   = new Button.t ~label:"применить" () in
      let box     = new Box.t ~widgets:[grid#widget] () in
      (* Card sections *)
      let actions = new Card.Actions.t ~widgets:[add#widget; rm#widget; edit#widget] () in
      let media   = new Card.Media.t ~widgets:[box] () in

      let ph      = make_placeholder ~text:"Добавьте элементы в раскладку" ~icon:"add_box" () in
      let _       = grid#add_class "wm-grid" in
      let _       = grid#set_on_load @@ Some (fun () -> grid#layout) in
      let _       = f_add add#e_click in
      let _       = f_rm rm#e_click in
      let _       = React.E.map (fun _ -> post { wm with layout = React.S.value layout }) apply#e_click in
      let _       = React.S.map (function
                                 | [] -> Dom.appendChild grid#root ph#root
                                 | _  -> try Dom.removeChild grid#root ph#root with _ -> ())
                                grid#s_items
      in
      let card    = new Card.t ~sections:[`Actions actions; `Media media] () in
      card#add_class "wm";
      card#set_id id;
      card
    in
    let _ = React.E.map (fun s ->
                (try Dom.removeChild div (Dom_html.getElementById id)
                 with _ -> print_endline "No el");
                Dom.appendChild div (make s)#root) events
    in
    Dom.appendChild div (make init)#root;
    grid#root

end

class t () =

  let elt = Dom_html.createDiv Dom_html.document in

  object(self)

    val mutable sock : WebSockets.webSocket Js.t option = None

    inherit Widget.widget elt () as super

    method private on_load =
      Requests.get_wm ()
      >>= (fun wm ->
        let e_wm,wm_sock = Requests.get_wm_socket () in
        let open Lwt.Infix in
        let wm_el = Wm.create ~init:wm ~events:e_wm
                              ~post:(fun w -> Requests.post_wm w
                                              >|= (function
                                                   | Ok () -> ()
                                                   | Error e -> print_endline @@ "error post wm" ^ e)
                                              |> Lwt.ignore_result)
        in
        sock <- Some wm_sock;
        Dom.appendChild self#root wm_el;
        Lwt_result.return ())
      |> ignore

    initializer
      super#set_on_unload @@ Some (fun () -> Option.iter (fun x -> x##close; sock <- None) sock);
      super#set_on_load   @@ Some (fun () -> self#on_load);

  end

let page () = new t ()
