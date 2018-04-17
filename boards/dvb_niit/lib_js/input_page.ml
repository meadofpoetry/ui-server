open Lwt_result.Infix
open Containers
open Components
open Measures

module Listener = Topo_page.Listener

let make_chart : type a. init:a data -> event:a data React.event -> a config -> Widget.widget =
  fun ~init ~event config ->
  let chart = make_chart ~init ~event config in
  let exp   = new Expansion_panel.t ~title:(chart_name_of_typ config.typ) ~content:[chart] () in
  exp#panel#style##.height := Js.string "250px";
  exp#set_expanded true;
  exp#widget

let fab_class = "mdc-fixed-fab"

let make_param w =
  new Card.t ~widgets:[w] ()

(* let add (grid:'a Dynamic_grid.t) widget =
 *   let open Dynamic_grid.Position in
 *   let w = grid#grid.cols in
 *   let y = List.fold_left (fun acc x -> if x#pos.y + x#pos.h > acc then x#pos.y + x#pos.h else acc) 0 grid#items in
 *   grid#add @@ Dynamic_grid.Item.to_item ~pos:{x=0;y;w;h=4} ~value:() ~widget () *)

let make (control:int) =
  let t = Listener.listen control
          >>= (fun (l,state) ->
      let modules = List.map fst l.config |> List.sort compare in
      let conf t = { typ = t; modules; duration = 120000L } in
      let mer_p = make_param @@ Widgets.Mer_param.make ~init:None ~event:(React.E.map (fun (_,(x:Board_types.measure)) -> x.mer) l.events.status) () in
      let pow  = make_chart ~init:[] ~event:(to_power_event l.events.status) (conf Power) in
      let mer  = make_chart ~init:[] ~event:(to_mer_event l.events.status) (conf Mer) in
      let ber  = make_chart ~init:[] ~event:(to_ber_event l.events.status) (conf Ber) in
      let frq  = make_chart ~init:[] ~event:(to_freq_event l.events.status) (conf Freq) in
      let br   = make_chart ~init:[] ~event:(to_bitrate_event l.events.status) (conf Bitrate) in
      let box  = new Box.t  ~widgets:[mer_p;pow;mer;ber;frq;br] () |> Widget.coerce in
      box#set_on_destroy @@ Some (fun () -> Listener.unlisten state);
      Lwt_result.return box)
  in
  object(self)
    inherit Widget.widget (Dom_html.createDiv Dom_html.document) ()
    method on_load = ()
    method on_unload = t >>= (fun w -> w#destroy; Lwt_result.return ()) |> Lwt.ignore_result
    initializer
      t >>= (fun w -> Dom.appendChild self#root w#root; Lwt_result.return ()) |> Lwt.ignore_result
  end
