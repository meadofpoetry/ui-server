open Containers
open Components

let make ?error_prefix (board:Common.Topology.topo_board) : (#Widget.widget,string) Lwt_result.t =
  let factory = new Widget_factory.t board.control () in
  let w = factory#create @@ Settings { ids = None } in
  w.widget#set_on_destroy @@ Some factory#destroy;
  Lwt_result.return w.widget
