open Lwt_result.Infix

let page control () =
  let factory = new Widget_factory.t control () in
  let w = factory#create (TS_log None) in
  w.widget#set_on_destroy (Some factory#destroy);
  w.widget
