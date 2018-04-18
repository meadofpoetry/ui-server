open Containers
open Components

let make ?error_prefix (board:Common.Topology.topo_board) : (#Widget.widget,string) Lwt_result.t =
  let factory = new Widgets.Factory.t board.control () in
  let w = factory#create Settings in
  w#set_on_destroy @@ Some (fun () -> factory#destroy);
  Lwt_result.return w
