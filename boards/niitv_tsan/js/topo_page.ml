open Components

let make (board : Common.Topology.topo_board) : (#Widget.t,string) Lwt_result.t =
  let factory = new Widget_factory.t board.control () in
  let w = factory#create @@ Settings None in
  w.widget#set_on_destroy factory#destroy;
  Lwt_result.return w.widget
