open Lwt_result.Infix

let page input control () =
  let factory = new Widget_factory.t control () in
  let w = factory#create (TS_log (Some { inputs = [ input ] })) in
  w.widget#set_on_destroy (Some factory#destroy);
  w.widget
